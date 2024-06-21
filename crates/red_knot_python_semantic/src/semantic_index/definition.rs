use crate::node_key::NodeKey;
use crate::semantic_index::ast_ids::{
    ScopedClassId, ScopedDefinitionNodeId, ScopedExpressionId, ScopedFunctionId,
};
use ruff_python_ast as ast;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Definition {
    ImportAlias(ScopedDefinitionNodeId),
    ClassDef(ScopedClassId),
    FunctionDef(ScopedFunctionId),
    Target(ScopedExpressionId),
    NamedExpr(ScopedExpressionId),
    /// represents the implicit initial definition of every name as "unbound"
    Unbound,
    // TODO with statements, except handlers, function args...
}

impl From<ScopedClassId> for Definition {
    fn from(value: ScopedClassId) -> Self {
        Self::ClassDef(value)
    }
}

impl From<ScopedFunctionId> for Definition {
    fn from(value: ScopedFunctionId) -> Self {
        Self::FunctionDef(value)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub(crate) enum DefinitionNodeKey {
    Function(NodeKey),
    Class(NodeKey),
    Alias(NodeKey),
}

impl From<&ast::StmtFunctionDef> for DefinitionNodeKey {
    fn from(value: &ast::StmtFunctionDef) -> Self {
        Self::Function(NodeKey::from_node(value))
    }
}

impl From<&ast::StmtClassDef> for DefinitionNodeKey {
    fn from(value: &ast::StmtClassDef) -> Self {
        Self::Class(NodeKey::from_node(value))
    }
}

impl From<&ast::Alias> for DefinitionNodeKey {
    fn from(value: &ast::Alias) -> Self {
        Self::Alias(NodeKey::from_node(value))
    }
}
