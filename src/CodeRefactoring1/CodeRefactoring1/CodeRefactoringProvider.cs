using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Formatting;

namespace CodeRefactoring1
{
    [ExportCodeRefactoringProvider(RefactoringId, LanguageNames.CSharp), Shared]
    internal class CodeRefactoring1CodeRefactoringProvider : CodeRefactoringProvider
    {
        public const string RefactoringId = "CodeRefactoring1";

        public sealed override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var node = root.FindNode(context.Span);
            var parameter = node as ParameterSyntax;
            if (parameter == null)
            {
                return;
            }

            var parameterName = RoslynHelpers.GetParameterName(parameter);
            var underscorePrefix = "_" + parameterName;
            var uppercase = parameterName.Substring(0, 1).ToUpper() + parameterName.Substring(1);

            if (RoslynHelpers.VariableExists(root, parameterName, underscorePrefix, uppercase))
            {
                return;
            }

            var action = CodeAction.Create(
                "Introduce and initialize field '" + underscorePrefix + "'",
                ct => CreateFieldAsync(context, parameter, parameterName, underscorePrefix, ct));

            context.RegisterRefactoring(action);
        }

        private async Task<Document> CreateFieldAsync(CodeRefactoringContext context, ParameterSyntax parameter,
            string paramName, string fieldName, CancellationToken cancellationToken)
        {
            var oldConstructor = parameter.Ancestors().OfType<ConstructorDeclarationSyntax>().First();
            var newConstructor = oldConstructor.WithBody(oldConstructor.Body.AddStatements(
                 SyntaxFactory.ExpressionStatement(
                     SyntaxFactory.AssignmentExpression(
                         SyntaxKind.SimpleAssignmentExpression,
                         SyntaxFactory.IdentifierName(fieldName),
                         SyntaxFactory.IdentifierName(paramName)))));

            var oldClass = parameter.FirstAncestorOrSelf<ClassDeclarationSyntax>();
            var oldClassWithNewCtor = oldClass.ReplaceNode(oldConstructor, newConstructor);

            var fieldDeclaration = RoslynHelpers.CreateFieldDeclaration(RoslynHelpers.GetParameterType(parameter), fieldName);
            var newClass = oldClassWithNewCtor
                .WithMembers(oldClassWithNewCtor.Members.Insert(0, fieldDeclaration))
                .WithAdditionalAnnotations(Formatter.Annotation);

            var oldRoot = await context.Document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            var newRoot = oldRoot.ReplaceNode(oldClass, newClass);

            return context.Document.WithSyntaxRoot(newRoot);
        }
    }
}