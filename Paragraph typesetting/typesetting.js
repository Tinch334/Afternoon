class Box {
    constructor(width) {
        this.width = width;
        Object.freeze(this); // Prevents accidental modification.
    }
}

class Glue {
    constructor(min, max, ideal) {
        this.min = min;
        this.max = max;
        this.ideal = ideal;
        this.current = ideal;
    }

    // These values are calculated with the current width, to be correct even when the glue is adjusted.
    get shrink() {
        return this.current - this.min;
    }

    get stretch() {
        return this.max - this.current;
    }
}


const canvas = document.getElementById("canvas");
const context = canvas.getContext("2d");


/*
function drawElems(x, y, height, elem) {
    if (elem instanceof Box) {
        context.fillStyle = "#132FBE";
        context.fillRect(x, y + height, elem.width, height);
    } else if (elem instanceof Glue) {
        context.fillStyle = "#6A81F0";
        context.fillRect(x, y + height, elem., height);
    }
}
*/

function drawElem(x, y, height, width, colour) {
    context.fillStyle = colour;
    context.fillRect(x, y, width, height);
}

function typeset(elems, width, height) {
    console.log(`w: ${width} - h: ${height}`);

    // Display variables.
    let boxHeight = 50;
    let interlineSpacing = 15;

    //Position variables.
    let line = 1;

    let currentLine = [];
    let boxWidth = 0;
    let glueWidth = 0;

    while (elems.length > 0) {
        let el = elems[0];

        if (el instanceof Box) {
            // Add box to line and remove it from the list.
            currentLine.push(el);
            boxWidth += el.width;
            elems.shift();
        } else if (el instanceof Glue) {
            currentLine.push(el);
            glueWidth += el.ideal;
            elems.shift();
        }

        let cl = currentLine.length;
        if (boxWidth + glueWidth > width) {
            // Check if the element causing a problem is a box, if it's a space we can just brake.
            if (currentLine[cl - 1] instanceof Box) {
                // Check if the line fits if the glue is shrunk.
                if (boxWidth + getListShrink(currentLine) <= width) {
                    // How much the current line is "over" the width.
                    let diff = (boxWidth + glueWidth) - width;
                    // The list might be iterated several times.
                    let i = 0;

                    while (diff > 0) {
                        if (currentLine[i] instanceof Glue) {
                            // Adjust width of glue, capped by diff.
                            let adj = Math.min(currentLine[i].shrink * 0.1, diff);
                            currentLine[i].current -= adj;
                            diff -= adj;
                        }

                        i += 1;
                        if (i >= cl) {
                            i = 0;
                        }
                    }
                } // Check if the line fits if the last word and it's accompanying glue are removed and the glues stretched.
                else if ((boxWidth - currentLine[cl - 1].width) + getListStretch(currentLine.slice(0, cl - 2)) >= width) {
                    // How much the current line is "under" the width.
                    let diff = width - ((boxWidth - currentLine[cl - 1].width) + (glueWidth - currentLine[cl - 2].current));

                    // Add last word to elements to be typeset.
                    elems.unshift(currentLine[cl - 1]);
                    // Remove last word and it's glue from elements being typeset.
                    currentLine.pop();
                    currentLine.pop();

                    // The list might be iterated several times.
                    let i = 0;

                    while (diff > 0) {
                        if (currentLine[i] instanceof Glue) {
                            // Adjust width of glue, capped by diff.
                            let adj = Math.min(currentLine[i].stretch * 0.2, diff);
                            currentLine[i].current += adj;
                            diff -= adj;
                        }

                        i += 1;
                        // cl is no longer accurate, cannot be used.
                        if (i >= currentLine.length) {
                            i = 0;
                        }
                    }
                } else {
                    console.log("Must split box");
                }
            }

            let cursor = 0;

            while (currentLine.length > 0) {
                if (currentLine[0] instanceof Box) {
                    let w = currentLine[0].width;
                    drawElem(cursor, line * boxHeight + (line - 1) * interlineSpacing, boxHeight, w, "#1A3EE8");
                    cursor += w;
                } else if (currentLine[0] instanceof Glue) {
                    let w = currentLine[0].current;
                    drawElem(cursor, line * boxHeight + (line - 1) * interlineSpacing, boxHeight, w, "#63CA7D");
                    cursor += w;
                }

                currentLine.shift();
            }

            // Reset line.
            currentLine = [];
            boxWidth = 0;
            glueWidth = 0;

            line += 1;
        }
    }

    let cursor = 0;
    while (currentLine.length > 0) {
        if (currentLine[0] instanceof Box) {
            let w = currentLine[0].width;
            drawElem(cursor, line * boxHeight + (line - 1) * interlineSpacing, boxHeight, w, "#1A3EE8");
            cursor += w;
        } else if (currentLine[0] instanceof Glue) {
            let w = currentLine[0].current;
            drawElem(cursor, line * boxHeight + (line - 1) * interlineSpacing, boxHeight, w, "#63CA7D");
            cursor += w;
        }

        currentLine.shift();
    }
}

function getListShrink(elems) {
    return elems.reduce(
        (acum, value) => {
            if (value instanceof Glue) {
                return acum + value.min
            } else { return acum }
        },
        0,
    );
}

function getListStretch(elems) {
    return elems.reduce(
        (acum, value) => {
            if (value instanceof Glue) {
                return acum + value.max
            } else { return acum }
        },
        0,
    );
}

// Generate list of elements to typeset.
const generateItems = (iterations) => {
    let list = [];

    for (let i = 0; i < iterations; i++) {
        list.push(
            new Box(100),
            new Glue(4, 7, 5),
            new Box(30),
            new Glue(4, 7, 5),
            new Box(50),
            new Glue(4, 8, 5),
            new Box(30),
            new Glue(6, 10, 8),
            new Box(55),
            new Glue(6, 10, 8),
            new Box(60)
        );
    }

    return list;
};

let items = generateItems(30);

context.beginPath();
context.moveTo(0, canvas.height * 0.9);
context.lineTo(canvas.width * 0.9, canvas.height * 0.9);
context.lineTo(canvas.width * 0.9, 0);
context.stroke();

typeset(items, canvas.width * 0.9, canvas.height * 0.9);