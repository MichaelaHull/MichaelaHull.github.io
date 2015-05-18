
/*
 * If there is an error, insert an error message in the HTML
 * and log the error to the console.
 */
function processError(error) {
    if (error) {
        // Use the "statusText" of the error if possible
        var errorText = error.hasOwnProperty("statusText") ?
            error.statusText : error.toString();

        // Insert the error message before all else
        d3.select("body")
            .insert("p", ":first-child")
            .text("Error: " + errorText)
            .style("color", "red");

        // Log the error to the console
        console.warn(error);
        return true;
    }

    return false;
}

/*
 * Parses us-state-names.tsv into components.
 * Used by d3.tsv() function.
*/
function parseStateName(row) {
    return {
        id: +row.id,
        name: row.name.trim(),
        code: row.code.trim().toUpperCase()
    };
}

function symbolMap() {

    var lookup = {};

    var width = 960;
    var height = 960;

    // var projection = d3.geo.mercator()
    //     .scale((width + 1) / 2 / Math.PI)
    //     .translate([width / 2, height / 2])
    //     .precision(0.1);

    var graticule = d3.geo.graticule();

    var radius = d3.scale.sqrt().range([5, 15]);

    var log = d3.select("#log");

    var map = null; // map data
    var values = null; // values for symbols

    // gets the value property from the dataset
    // for our example, we need to reset this!
    var value = function(d) { return d.value; };

    function chart(id) {
        if (map === null || values === null) {
            console.warn("Unable to draw symbol map: missing data.");
            return;
        }

        updateLog("Drawing map... please wait.");

        var svg = d3.select("svg#" + id);
        var bbox = svg.node().getBoundingClientRect();
        //map x domain: [-122.513642064265, -122.365240723693]
        //map y domain: [37.7079219034586, 37.819975492297]

        // update project scale
        // (this may need to be customized for different projections)
        var projection = d3.geo.mercator()//projection.scale(bbox.width);
            .scale((bbox.width - 2) / 2 / Math.PI)
            .translate([bbox.width / 2 + 10, bbox.height / 2 + 10])
            .precision(0.1);

        // update projection translation
        // projection = projection.translate([
        //     bbox.width / 2,
        //     bbox.height / 2
        // ]);

        // set path generator based on projection
        var path = d3.geo.path().projection(projection);

        // update radius domain
        // uses our value function to get the right property
        radius = radius.domain(d3.extent(values, value));

        // create groups for each of our components
        // this just reduces our search time for specific states
        var country = svg.append("g").attr("id", "country");
 //       var states  = svg.append("g").attr("id", "states");
        var symbols = svg.append("g").attr("id", "dots");

        // show that only 1 feature for land
        console.log(topojson.feature(map, map.objects.land));

        // show that we have an array of features for states
 //       console.log(topojson.feature(map, map.objects.states));

        // draw base map
        country.append("path")
            // use datum here because we only have 1 feature,
            // not an array of features (needed for data() call)
            .datum(topojson.feature(map, map.objects.land))
            .attr("d", path)
            .attr("fill", "#C2D1D9")
            .classed({"country": true});

        // draw states (invisible for now)
        // may need to adjust to draw countries instead?
        // states.selectAll("path")
        //     .data(topojson.feature(map, map.objects.states).features)
        //     .enter()
        //     .append("path")
        //     .attr("d", path)
        //     // set the ID so we can select it later
        //     .attr("id", function(d) { return "state" + d.id; })
        //     .classed({"state": true});

        // draw symbols
        var crimes = ["Theft", "Other", "Vandalism", "Assault", "Controlled", "Kidnapping"];
        var color = d3.scale.ordinal()
            .domain(crimes)
            .range(["#e6ab02", "#666666", "#1b9e77", "#e7298a", "#7570b3", "#17becf"]);

        symbols.selectAll("circle")
            .data(values)
            .enter()
            .append("circle")
            .attr("r", 2)
            .attr("cx", function (d, i) {
                // projection takes [longitude, latitude]
                // and returns [x, y] as output
                return projection([d.X, d.Y])[0];
            })
            .attr("cy", function (d, i) {
                return projection([d.X, d.Y])[1];
            })
            .style("fill", function (d, i) {
                return color(d.gen_cat);
            })
            .style("opacity", 0.1)
            .classed({"symbol": true})
            // .on("mouseover", showHighlight)
            // .on("mouseout", hideHighlight);
    }

    /*
     * These are functions for getting and setting values.
     * If no argument is provided, the function returns the
     * current value. Otherwise, it sets the value.
     *
     * If setting the value, ALWAYS return the chart object.
     * This will allow you to save the updated version of
     * this environment.
     *
     * Personally, I do not like _ to indicate the argument
     * that may or may not be provided, but its what the
     * original model uses.
     */

    // gets/sets the mapping from state abbreviation to topojson id
    chart.lookup = function(_) {
        // if no arguments, return current value
        if (!arguments.length) {
            return lookup;
        }

        // otherwise assume argument is our lookup data
        _.forEach(function(element) {
            lookup[element.id] = element.name;

            // lets you lookup the ID of a state
            // by its code (2-letter abbreviation)
            if (element.hasOwnProperty("code")) {
                lookup[element.code] = element.id;
            }
        });

        // always return chart object here
        console.log("Updated lookup information.")
        return chart;
    };

    /*
     * Note the semi-colon above. This was an assignment,
     * even though we were defining a function. All assignments
     * should end in a semi-colon.
     */

    // allows for customization of projection used
    chart.projection = function(_) {
        if (!arguments.length) {
            return projection;
        }

        projection = _;
        return chart;
    };

    /*
     * You can get/set functions just like variables.
     * The basic format is always the same.
     */

    // allows for customization of radius scale
    chart.radius = function(_) {
        if (!arguments.length) {
            return radius;
        }

        radius = _;
        return chart;
    };

    // updates the map data, must happen before drawing
    chart.map = function(_) {
        if (!arguments.length) {
            return map;
        }

        map = _;
        updateLog("Map data loaded.");

        return chart;
    };

    // updates the symbols values, must happen before drawing
    chart.values = function(_) {
        if (!arguments.length) {
            return values;
        }

        values = _;
        updateLog("Symbol data loaded.");

        return chart;
    };

    // updates how we access values from our dataset
    chart.value = function(_) {
        if (!arguments.length) {
            return value;
        }

        value = _;
        return chart;
    };

    /*
     * These functions are not outwardly accessible. They
     * are only used within this environment.
     */

    // updates the log message
    function updateLog(message) {
        // if no arguments, use default message
        if (!arguments.length) {
            log.text("Hover over a circle for more details");
            return;
        }

        // otherwise set log message
        log.text(message);
    }

    // called on mouseover
    function showHighlight(d) {
        // highlight symbol
        d3.select(this).classed({
            "highlight": true,
            "symbol": true
        });

        // highlight state associated with symbol

        updateLog(d.place +
            " experienced a magnitude " + d.mag +
            " earthquake at a depth of " + d.depth + ".");
    }

    // called on mouseout
    function hideHighlight(d) {
        // reset symbol
        d3.select(this).classed({
            "highlight": false,
            "symbol": true
        });

        // reset state associated with symbol
        d3.select("g#states")
            .select("path#state" + lookup[d.state])
            .classed({
                "highlight": false,
                "state": true
            });

        // reset log message
        updateLog();
    }

    return chart;
}
