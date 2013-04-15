/**
 * Node.js script that calculate a lot of Dreadball dice stats and creates a formatted HTML page in /output
 */
//require.paths.unshift('./node_modules');

var fs = require('fs');
var _ = require('underscore');
var sys = require('sys');
var exec = require('child_process').exec;
var child;

// Configure combinations
var noDice = { from: 1, to: 6};
var targets = { from: 3, to: 5};

// Create all tables
var tables = [];
var resultsNeeded = Math.pow(targets.to - targets.from + 1, 2);
for (var i = targets.from; i <= targets.to; i++) {
    for (var j = targets.from; j <= targets.to; j++) {
        var table = createTable(i,j, function(table, top, left) {
            tables.push(table);
            if (tables.length == resultsNeeded) {
                console.log("Writing tables");
                saveResult(tables);
            }
        });
    }
}

function createTable(x,y, onSuccess) {
    // Create labels
    var labels = [];
    for (var k = noDice.from; k <= noDice.to; k++) {
        labels.push(k);
    }

    // Create default table
    var table = {
        top: x,
        left: y,
        labels: {
            top: labels,
            left: labels
        },
        rows: []
    };

    // Calculate all cells
    var dice = noDice.to - noDice.from;
    var cellsNeeded = Math.pow(noDice.to - noDice.from + 1, 2);

    var cellDone = function() {
        cellsNeeded--;
        if (cellsNeeded == 0) {
            onSuccess(table, x, y);
        }
    };

    for (var l = 0; l <= dice; l++) {
        for (var m = 0; m <= dice; m++) {
            createCell(x,y,l,m, function(cell, rowNo, columnNo) {
                if (table.rows[rowNo] == undefined) {
                    table.rows[rowNo] = [];
                }

                var row = table.rows[rowNo];
                row[columnNo] = cell;
                cellDone();
            });
        }
    }
}

function createCell(targetX, targetY, diceX, diceY, onSuccess) {

    var cmd = "etc/troll -0 etc/dreadball-compare.troll TARGETX=" + targetX + " TARGETY=" +targetY + " DICEX=" + (diceX + 1) + " DICEY=" + (diceY + 1);
    child = exec(cmd, function (error, stdout, stderr) {
        if (error != null) {
            throw error;
            return;
        }

        process.stdout.write(".");
        onSuccess(parseCompareStats(stdout), diceX, diceY);
    });
}







/**
 * Parses output like
 * ----
 * Value    % =                 % >=
 * -1 :   12.1873953142       99.9999996325
 * 0 :   12.2791548218       87.8126043183
 * 1 :   18.726180436        75.5334494965
 * 2 :   18.9608542136       56.8072690605
 * 3 :   37.8464148468       37.8464148468
 *
 * Average = 1.5799973809    Spread = 1.40618474555  Mean deviation = 1.23411232692
 * -----
 *
 * @param trollOutput
 */
function parseCompareStats(trollOutput) {

    var result = {
        atLeast : {
            doubleSuccess : null,
            success : null,
            draw : null,
            failure : null,
            doubleFailure : null
        },
        group : {
            doubleSuccess : null,
            success : null,
            draw : null,
            failure : null,
            doubleFailure : null
        }
    }

    var pattern = /\s*[0-9-]*\s*:\s*([0-9.]*)\s*([0-9.]*)\s*/i;
    var lines = trollOutput.split("\n");
    for (var i = 1; i < 6; i++) {
        var matches = pattern.exec(lines[i]);
        var percentage = parseFloat(matches[1]);
        var totalPercentage = parseFloat(matches[2]);

        switch(i) {
            case 5:
                result.group.doubleFailure = percentage;
                result.atLeast.doubleFailure = 100 - (totalPercentage - percentage);
                break;

            case 4:
                result.group.failure = percentage;
                result.atLeast.failure = 100 - (totalPercentage - percentage);
                break;

            case 3:
                result.group.draw = percentage;
                result.atLeast.draw = 100 - (totalPercentage - percentage);
                break;

            case 2:
                result.group.success = percentage;
                result.atLeast.success = 100 - (totalPercentage - percentage);
                break;

            case 1:
                result.group.doubleSuccess = percentage;
                result.atLeast.doubleSuccess = 100 - (totalPercentage - percentage);
                break;
        }
    }

    return result;
}

/**
 * Save the calculated stats to the output file
 * @param result
 */
function saveResult(result) {

    var indexTemplate = _.template(fs.readFileSync('template/index.html', 'utf-8'));
    var tableTemplate = _.template(fs.readFileSync('template/table.html', 'utf-8'));
    var statsTemplate = _.template(fs.readFileSync('template/stats.html', 'utf-8'));

    var tables = [];
    for (var i = 0; i < result.length; i++) {
        var table = result[i];

        // Compile stats
        for (var j = 0; j < table.rows.length; j++) {
            var row = table.rows[j];
            for (var k = 0; k < row.length; k++) {
                var cell = row[k];
                table.rows[j][k] = statsTemplate({top: cell.atLeast.doubleSuccess,  middle: cell.atLeast.success, bottom: cell.atLeast.draw});
            }
        }
        // Compile table
        tables.push(tableTemplate(table));
    };

    // Compile index
    var result = indexTemplate({'tables': tables});

    fs.writeFile("output/index.html", result, function(err) {
        if(err) {
            console.log(err);
        } else {
            console.log("index.html was generated correctly.");
        }
    });
}



