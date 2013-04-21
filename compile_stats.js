
var DREADBALL = (function() {

    var SUCCESSES = require('./compile_successes');
    var OPPOSED_ROLLS = require('./compile_opposed_rolls');
    var fs = require('fs');
    var _ = require('underscore');

    // Map for table results
    var result = {}

    function saveResult(result) {
        var indexTemplate = _.template(fs.readFileSync('template/index.html', 'utf-8'));

        fs.writeFile("output/index.html", indexTemplate(result), function(err) {
            if(err) {
                console.log(err);
            } else {
                console.log("\nindex.html was generated correctly.");
            }
        });
    }

    return {
        /**
         * Compile dice stats and put results in /output/index.html
         */
        compileStats : function() {
            var successTables = SUCCESSES.start(function(tables) {
                result['successes'] = tables;
                if (result['opposedRolls'] != undefined) {
                    saveResult(result);
                }
            });

            var opposedRolls = OPPOSED_ROLLS.start(function(tables) {
                result['opposedRolls'] = tables;
                if (result['successes'] != undefined) {
                    saveResult(result);
                }
            });
        }
    };
})();

DREADBALL.compileStats();