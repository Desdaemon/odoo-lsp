use futures::stream::FuturesOrdered;

use super::*;

pub struct Method {
	pub name: String,
}

query! {
	MethodQuery();
	""
}

impl ModelEntry {
	pub fn model_methods(&self, index: &ModelIndex, prefix: &str) -> impl futures::Stream<Item = Method> + '_ {
		let what = (self.base.iter().chain(self.descendants.iter()))
			.map(|loc| async {
				let contents = tokio::fs::read(loc.0.path.to_path()).await.unwrap_or_default();
				(loc, contents)
			})
			.collect::<FuturesOrdered<_>>();

		let ancestors = self
			.ancestors
			.iter()
			.flat_map(|ancestor| Some(index.get(ancestor)?.model_methods(index, prefix)));

		todo!()
		// let stream = (self.base.iter().chain(self.descendants.iter()))
		// 	.map(|loc| async move {
		// 		let contents = tokio::fs::read(loc.0.path.to_path()).await.unwrap_or_default();
		// 		(loc, contents)
		// 	})
		// 	.collect::<FuturesUnordered<_>>();
		// stream.flat_map(|(loc, contents)| {
		// 	let mut methods = vec![];
		// 	methods.push(Method {
		// 		name: "what".to_string(),
		// 	});
		// 	futures::stream::iter(methods)
		// })
	}
}
