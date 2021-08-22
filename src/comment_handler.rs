use ress::prelude::*;

/// A comment handler will allow you to specify
/// behavior about what to do with comments
/// officially comments are supposed to operate
/// the same as whitespace so the default behavior
/// would be to throw away any comments found
pub trait CommentHandler<'a> {
    fn handle_comment(&mut self, comment: Item<&'a str>);
}
/// The default comment handler,
/// this will discard comments
/// provided to it
pub struct DefaultCommentHandler;

impl<'a> CommentHandler<'a> for DefaultCommentHandler {
    fn handle_comment(&mut self, _: Item<&'a str>) {}
}

impl<'a, F> CommentHandler<'a> for F
where
    F: FnMut(Item<&'a str>),
{
    fn handle_comment(&mut self, item: Item<&'a str>) {
        self(item)
    }
}
