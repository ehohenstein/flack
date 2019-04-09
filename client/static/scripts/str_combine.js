
function combine(strings) {
    // Yes, this algorithm is very English dependendent. It does not lend itself well
    // to localization.
    if (strings.length > 0) {
        let first = strings.slice(0, strings.length - 1);
        let last = strings[strings.length - 1];
        let combined = '';
        if (first.length === 1) {
            combined = first[0] + ' and ';
        } else if (first.length > 1) {
            combined = first.join(', ') + ', and ';
        }
        combined += last;
        return combined;
    }
    return null;
}

module.exports = combine;
