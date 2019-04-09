
function toColor(hex) {
    let value = Math.floor(parseInt(hex, 16) / 2);
    let color = value.toString(16);
    if (value < 16) {
        color = '0' + color;
    }
    return color;
}

function user_color(user_id) {
    // This assumes that the user_id is a string of random hex characters.
    // The first 6 characters are used as the color by treating them as
    // 3 bytes, converting them to integers, and then dividing them by 2
    // to get 3 new colors that will always be in the bottom half of the
    // color range so as to contrast sufficiently with the light background.
    let red = toColor(user_id.substring(0, 2));
    let green = toColor(user_id.substring(2, 4));
    let blue = toColor(user_id.substring(4, 6));
    return '#' + red + green + blue;
}

module.exports = user_color;
