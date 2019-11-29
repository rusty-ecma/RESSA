function addNumbers() {
    const codeEl = document.querySelector('.language-js');
    let lineCount = codeEl.textContent.split('\n').length;
    let gutterWidth = lineCount > 99 ? 50 : 25;
    const gutter = document.createElement('div');
    gutter.setAttribute('class', 'code-gutter')
    document.body.appendChild(gutter)
    gutter.style.top = codeEl.offsetTop;
    gutter.style.height = codeEl.clientHeight;
    gutter.style.top = codeEl.offsetTop;
    gutter.style.left = codeEl.offsetLeft - gutterWidth;
    gutter.style.width = `${gutterWidth}px`;
    for (var i = 0; i < lineCount; i++) {
        let num = document.createElement('span')
        num.innerText = `${i + 1}.`;
        num.classList.add('gutter-number');
        gutter.appendChild(num);
    }
};
function highlight() {
    const blocks = document.querySelectorAll('pre code')
    for (const block of blocks) {
        hljs.highlightBlock(block);
    }
}

window.addEventListener('DOMContentLoaded', () => {
    highlight();
    addNumbers();
});