/**
 * Node.js script that calculates the expected number of success for a given dice roll.
 */

module.exports = {
    start : function (onSuccess) {
        createTables(function(tables) {
            onSuccess(tables);
        });
    }
};

var fs = require('fs');
var _ = require('underscore');
var sys = require('sys');
var exec = require('child_process').exec;
var async = require('async');

var child;

// Configure combinations
var noDice = { from: 1, to: 6};
var targets = { from: 3, to: 5};

// Create all tables
function createTables(onSuccess) {
    var tables = [];
    var tablesNeeeded = noDice.to - noDice.from + 1;
    for (var i = noDice.from; i <= noDice.to; i++) {
        createTable(i, function(table, noOfDice) {
            tables[noOfDice - noDice.from] = table;
            tablesNeeeded--;

            if (tablesNeeeded == 0) {
                onSuccess(saveResult(tables));
            }
        });
    }
}

function createTable(noOfDice, onSuccess) {
    // Create labels
    var topLabels = [];
    for (var i = targets.from; i <= targets.to; i++) {
        topLabels.push(i + "+");
    }

    // Create default table
    var table = {
        info: noOfDice + " dice",
        labels: {
            top: topLabels
        },
        cols: []
    };

    var tasks = []
    for (var i = targets.from; i <= targets.to; i++) {
        tasks.push(function(i) {
            return function(callback) {
                calculateSucccessForDiceRoll(noOfDice, i, function(result, noOfDice, target) {
                    var col = [];
                    var seenZero = false;
                    for (var j = 0; j < result.length; j++) {
                        var percentage = Math.round(result[j].atLeast);
                        if (seenZero && percentage == 0) break;
                        col.push(percentage);
                        if (percentage == 0) seenZero = true;
                    }

                    var colNo = target - targets.from;
                    if (table.cols[colNo] == undefined) {
                        table.cols[colNo] = [];
                    }

                    table.cols[colNo] = col;
                    callback(null, col);
                });
            }
        }(i));

    }

    async.series(tasks, function(err, results) {
        onSuccess(table, noOfDice);
    });
}

function calculateSucccessForDiceRoll(noOfDice, target, onSuccess) {
    var cmd = "./etc/troll -0 etc/dreadball-success.troll DICE=" + noOfDice + " TARGET=" + target;
    child = exec(cmd, function (error, stdout, stderr) {
        if (error != null) {
            throw error;
            return;
        }

        process.stdout.write(".");
        onSuccess(parseStats(stdout), noOfDice, target);
    });
}

/**
 * Parses output like
 * ----
 * Value    % =                 % >=
 * 0 :   66.6666666667       99.9999999541
 * 1 :   27.7777777778       33.3333332874
 * 2 :    4.62962962963       5.55555550962
 * 3 :    0.771604938272      0.925925879987
 * 4 :    0.128600823045      0.154320941715
 * 5 :    0.0214334705075     0.0257201186697
 * 6 :    0.00357224508459    0.00428664816214
 * 7 :    0.000595374180765   0.000714403077552
 * 8 :    9.92290301275E~5    0.000119028896787
 * 9 :    1.65381716879E~5    1.97998666597E~5
 * 10 :    2.75636194799E~6    3.26169497178E~6
 * 11 :    4.59393657998E~7    5.05333023798E~7
 * 12 :    4.59393657998E~8    4.59393657998E~8
 *
 * Average = 0.399999994304    Spread = 0.632455479648  Mean deviation = 0.533333325922
 * -----
 *
 * @param trollOutput
 */
function parseStats(trollOutput) {

    var result = [];

    var pattern = /\s*([0-9-]*)\s*:\s*([0-9.]*)\s*([0-9.]*)\s*/i;
    var lines = trollOutput.split("\n");
    for (var i = 1; i < lines.length; i++) {
        var matches = pattern.exec(lines[i]);
        if (matches === null || matches.length != 4) break; // break at newline

        var successDice = parseInt(matches[1]);
        var percentage = parseFloat(matches[2]);
        var totalPercentage = parseFloat(matches[3]);
        var diceResult = {'dice': successDice, 'group': percentage, 'atLeast': totalPercentage};
        result.push(diceResult);
    }

    return result;
}

/**
 * Save the calculated stats to the output file
 * @param result
 */
function saveResult(result) {

    var tableTemplate = _.template(fs.readFileSync('template/table_success.html', 'utf-8'));
    var statsTemplate = _.template(fs.readFileSync('template/stats.html', 'utf-8'));

    var tables = [];
    for (var i = 0; i < result.length; i++) {
        var table = result[i];
        tables.push(tableTemplate(table));
    }

    return tables;
}