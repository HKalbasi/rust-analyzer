use std::{cell::RefCell, path::PathBuf};

use ide_db::base_db::FileId;
use linter_adapter::context::{DriverContext, DriverContextWrapper};
use linter_api::{
    ast::{
        item::{self, CommonItemData, ItemId, ItemType, ModItem, StaticItem},
        ty::Mutability,
        BodyId, Crate, CrateId, Span, Symbol,
    },
    context::AstContext,
};
use once_cell::unsync::OnceCell;
use syntax::{
    ast::{HasModuleItem, HasVisibility, Item},
    SourceFile, SyntaxNode, TextRange, TextSize, AstNode,
};

use crate::{Diagnostic, DiagnosticsConfig};

struct RADriver<'a> {
    diags: RefCell<&'a mut Vec<Diagnostic>>,
    id_map: RefCell<&'a mut Vec<SyntaxNode>>,
    ast_context: OnceCell<&'a AstContext<'a>>,
}

fn leak<'a, T: 'a>(x: T) -> &'a T {
    Box::leak(Box::new(x))
}

impl<'ast> DriverContext<'ast> for RADriver<'ast> {
    fn emit_lint(
        &self,
        lint: &'static linter_api::lint::Lint,
        msg: &str,
        span: &linter_api::ast::Span<'ast>,
    ) {
        self.diags.borrow_mut().push(Diagnostic::new(
            lint.name,
            msg,
            TextRange::new(TextSize::from(span.start() as u32), TextSize::from(span.end() as u32)),
        ));
    }

    fn get_span(
        &'ast self,
        owner: &linter_api::ast::SpanOwner,
    ) -> &'ast linter_api::ast::Span<'ast> {
        match owner {
            linter_api::ast::SpanOwner::Item(id) => {
                let span = self.id_map.borrow().get(id.get_data().1 as usize).unwrap().text_range();
                leak(Span::new(
                    self.ast_context.get().unwrap(),
                    linter_api::ast::SpanSource::File(leak(PathBuf::try_from("/").unwrap())),
                    span.start().into(),
                    span.end().into(),
                ))
            }
            linter_api::ast::SpanOwner::Body(_) => todo!(),
            linter_api::ast::SpanOwner::SpecificSpan(_) => todo!(),
        }
    }

    fn span_snippet(&self, _: &linter_api::ast::Span<'_>) -> Option<&'ast str> {
        todo!()
    }
}

pub(crate) fn external_lints(
    acc: &mut Vec<Diagnostic>,
    _: FileId,
    node: &SourceFile,
    config: &DiagnosticsConfig,
) {
    std::env::set_var("LINTER_LINT_CRATES", &config.external_lints);
    let mut x = linter_adapter::Adapter::new_from_env();
    let mut id_map = Vec::new();
    let driver = RADriver {
        diags: RefCell::new(acc),
        id_map: RefCell::new(&mut id_map),
        ast_context: OnceCell::new(),
    };
    let cw = DriverContextWrapper::new(&driver);
    let c = cw.create_driver_callback();
    let a = AstContext::new(&c);
    driver.ast_context.set(&a).unwrap();
    let crate_id = CrateId::new(0);
    let items: Vec<_> =
        node.items().filter_map(|item| convert_item(item, &driver, crate_id)).collect();
    x.process_krate(&a, &Crate::new(crate_id, &items));
}

fn convert_item<'ast>(
    item: Item,
    driver: &'ast RADriver<'ast>,
    crate_id: CrateId,
) -> Option<ItemType<'ast>> {
    let cx = &driver.ast_context.get().unwrap();
    let item_id = ItemId::new(crate_id, driver.id_map.borrow().len() as u32);
    driver.id_map.borrow_mut().push(item.syntax().clone());
    match item {
        Item::Module(m) => Some(ItemType::Mod(leak(ModItem::new(
            CommonItemData::new(
                cx,
                item_id,
                match m.visibility() {
                    Some(_) => item::Visibility::Pub,
                    None => item::Visibility::None,
                },
                Some(Symbol::new(5)),
            ),
            leak(match m.item_list() {
                Some(x) => {
                    x.items().filter_map(|item| convert_item(item, &driver, crate_id)).collect()
                }
                None => vec![],
            }),
        )))),
        Item::Static(st) => Some(ItemType::Static(leak(StaticItem::new(
            CommonItemData::new(
                cx,
                item_id,
                match st.visibility() {
                    Some(_) => item::Visibility::Pub,
                    None => item::Visibility::None,
                },
                Some(Symbol::new(5)),
            ),
            if st.mut_token().is_some() { Mutability::Mut } else { Mutability::Not },
            BodyId::new(2, 5135),
        )))),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::check_diagnostics;

    #[test]
    fn test_check_expr_field_shorthand() {
        check_diagnostics(
            r#"
struct A { a: &'static str }

mod foo {
    static X: u32 = 5;
}

   static X: u32 = 5;
 //^^^^^^^^^^^^^^^^^^ error: hey there is a static item here
fn main() { A { a: "hello" }; }
"#,
        );
    }
}
