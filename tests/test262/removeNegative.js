(function() {
    let button = document.getElementById('remove-neg-button');
    if (!button) {
        return console.error('unable to find button');
    }
    let hidden = false;
    button.addEventListener('click', () => {
        let negs = document.querySelectorAll('.negative');
        for (var i = 0; i < negs.length; i++) {
            negs[i].style.display = 'none';
        }
        let lis = document.querySelectorAll('.single-error-list');
        for (var i = 0; i < lis.length; i++) {
            let item = lis[i];
            let posCount = item.querySelectorAll('.positive').length;
            if(posCount === 0) {
                item.style.display = 'none';
            } else {
                console.log('positive not 0', posCount);
            }
        }
        let positiveCount = document.querySelectorAll('.positive').length;
        let report = document.querySelector('quote');
        report.innerText += ' positive: ' + positiveCount;
    });
})();