use async_lsp::lsp_types::Range;

use crate::index::{Symbol, SymbolMap, TemplateIndex};
use crate::template::TemplateName;
use crate::utils::MinLoc;

pub type ComponentName = Symbol<Component>;

#[derive(Default, Debug)]
pub struct Component {
	pub location: Option<MinLoc>,
	pub subcomponents: Vec<ComponentName>,
	pub props: SymbolMap<Prop, PropDescriptor>,
	/// Ancestors whose props are considered part of self.
	pub ancestors: Vec<ComponentName>,
	/// Extended as part of normal inheritance.
	pub extends: Option<ComponentName>,
	pub template: Option<ComponentTemplate>,
}

#[derive(Debug)]
pub enum ComponentTemplate {
	Name(TemplateName),
	Inline(Range),
}

impl Component {
	pub fn template_location(&self, templates: &TemplateIndex) -> Option<MinLoc> {
		let template = self.template.as_ref()?;
		match template {
			ComponentTemplate::Name(name) => templates.get(name)?.location.clone(),
			ComponentTemplate::Inline(range) => {
				let path = self.location.as_ref()?.path;
				Some(MinLoc { path, range: *range })
			}
		}
	}
}

#[derive(Debug)]
pub enum Prop {}

#[derive(Debug, Clone)]
pub struct PropDescriptor {
	pub location: MinLoc,
	pub type_: PropType,
}

bitflags::bitflags! {
	#[derive(Default, Copy, Clone, Debug, PartialEq, Eq)]
	pub struct PropType: u8 {
		const  Unknown = 0;
		const Optional = 1 << 0;
		const   String = 1 << 1;
		const   Number = 1 << 2;
		const  Boolean = 1 << 3;
		const   Object = 1 << 4;
		const Function = 1 << 5;
		const    Array = 1 << 6;

		const Any = !0;
	}
}
