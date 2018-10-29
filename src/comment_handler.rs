use ress::Item;
/// A comment handler will allow you to specify
/// behavior about what to do with comments
/// officially comments are supposed to operate
/// the same as whitespace so the default behavior
/// would be to throw away any comments found
pub trait CommentHandler {
    fn handle_comment(&mut self, comment: Item);
}
/// The default comment handler,
/// this will discard comments
/// provided to it
pub struct DefaultCommentHandler;

impl CommentHandler for DefaultCommentHandler {
    fn handle_comment(&mut self, _: Item) {}
}