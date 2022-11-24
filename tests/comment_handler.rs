use ress::prelude::*;
#[test]
fn comment_handler1() {
    let js = "
/**
 * @param s {string} The string to trim
 * @returns {string}
 */
function trimString(s) {
    return s.trim();
}
";
    let mut docs = vec![];
    let ch = |comments: Item<&str>| {
        if let Token::Comment(comment) = comments.token {
            docs.push(comment.to_string())
        }
    };
    let _ = ressa::Parser::builder()
        .js(js)
        .with_comment_handler(ch)
        .expect("failed to create parser")
        .parse()
        .expect("failed to parse js");
    assert!(docs.len() == 1, "docs not updated for comment");
    assert_eq!(
        docs[0],
        "/**
 * @param s {string} The string to trim
 * @returns {string}
 */"
    );
}

#[test]
fn comment_handler2() {
    let js = "
/**
 * @param s {string} The string to trim
 * @returns {string}
 */
function trimString(s) {
    /**
     * @param s {string} The string to trim
     * @returns {string}
     */
    function trimStringInner(s) {
        return s.trim();
    }
    return trimStringInner(s);
}";
    let mut docs = vec![];
    let ch = |comments: Item<&str>| {
        if let Token::Comment(comment) = comments.token {
            docs.push(comment.to_string())
        }
    };
    let _ = ressa::Parser::builder()
        .js(js)
        .with_comment_handler(ch)
        .expect("failed to create parser")
        .parse()
        .expect("failed to parse js");
    assert!(docs.len() == 2, "docs not updated for comment");
    assert_eq!(
        docs[0],
        "/**
 * @param s {string} The string to trim
 * @returns {string}
 */"
    );
    assert_eq!(
        docs[1],
        "/**
     * @param s {string} The string to trim
     * @returns {string}
     */"
    );
}

#[test]
fn comment_handler3() {
    use ressa::CommentHandler;
    #[derive(Clone)]
    struct Ch {
        pub comments: Vec<(SourceLocation, String)>,
    }
    impl Ch {
        fn new() -> Self {
            Self { comments: vec![] }
        }
    }
    impl<'a> CommentHandler<'a> for Ch {
        fn handle_comment(&mut self, comment: Item<&'a str>) {
            let loc = comment.location;
            if let Token::Comment(comment) = comment.token {
                let s = comment.to_string();
                self.comments.push((loc, s))
            }
        }
    }
    let js = "
/**
 * @param s {string} The string to trim
 * @returns {string}
 */
function trimString(s) {
    /**
     * @param s {string} The string to trim
     * @returns {string}
     */
    function trimStringInner(s) {
        return s.trim();
    }
    return trimStringInner(s);
}";
    let ch = Ch::new();
    let mut p = ressa::Parser::builder()
        .js(js)
        .with_comment_handler(ch)
        .expect("failed to create parser");
    let _res = p.parse().expect("failed to parse js");
    assert_eq!(p.comment_handler().comments.len(), 2);
    assert_eq!(
        p.comment_handler().comments[0].1,
        "/**
 * @param s {string} The string to trim
 * @returns {string}
 */"
    );
    assert_eq!(
        p.comment_handler().comments[1].1,
        "/**
     * @param s {string} The string to trim
     * @returns {string}
     */"
    );
}
