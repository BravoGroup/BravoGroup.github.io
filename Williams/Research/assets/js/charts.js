//Set initial property
        var prop = 'jobs'
        var filters = [];
        var chartTypePct = false;

        var chartTitles = {
            jobs : "How important is it that the pipeline project creates jobs for Pennsylvanians?",
            paenergy : "How important is it that the pipeline project uses Pennsylvania natural resources and helps 'put PA energy to work'?",
            indepsec : "How important is it that the pipeline project enchances American energy independence and security?",
            ussteel : "How important is it that the pipeline project uses steel manufactured in the United States in its construction?",
            cultural : "How important is it that the pipeline project route avoids Native American and other cultural and archaeological grounds?",
            envt : "How important is it that the pipeline project minimizes impact to wildlife and the environment?",
            farmland : "How important is it that the pipeline project minimizes impacts to farmland and agriculture?",
            safety : "How important is it that the pipeline project employs the highest safety standards for workers and residents?",
            renewables : "How important is it that the pipeline project helps to complement and support the growth of renewabless",
            steeldemand : "Demand for U.S. Steel too High",
            steelcapable : "Limited Steel Capabilities",
            farmuse : "Accuracy: After construction of the pipeline, most uses of the surface of the land will be allowed, including farming activities such as crop production and raising livestock",
            useexcept : "Accuracy: After construction of the pipeline, two notable exceptions to using the land include planting trees within the easement and placing a permanent structure within the easement",
            ageffect : "Accuracy: There are plans in place during and after pipeline construction to minimize effects on agricultural lands, and to ensure they are restored to their original uses and crop yields",
            farmvsag : "Agree/Disagree: Preserved farmland should have the same eligibility as other agricultural land when it comes to deciding where an energy infrastructure project is built",
            asr_feel : "Pre: How strongly do you support or oppose the Atlantic Sunrise pipeline project?",
            supp_cultural : "Change in Support: Williams has worked closely with FERC and the Pennsylvania Historical and Museum Commission (PHMC) to identify and consult with recognized Native American tribes in the project area",
            supp_archeo : "Change in Support: The Atlantic Sunrise project has undergone significant changes to avoid significant prehistoric and archaeological sites and aboveground resources",
            supp_NA : "Change in Support:  Williams commits to avoid cemeteries of any age, ethnic affiliation or type",
            asr_feel2 : "Post: Based on what you have just learned, how strongly do you support or oppose the Atlantic Sunrise Pipeline Project?",
            reliable_media : "For information about energy infrastructure projects, which of the following do you consider to be the most credible source?",
            benefit : "An important part of energy infrastructure projects is to assist in the development of activities the that directly benefit local communities. Which cause is most important to your region?"
        }

        //Utility functions
        function encodeKey(key) {
            return key.replace(/ /g, '_');
        }

        function decodeKey(key) {
            return key.replace(/_/g, ' ');
        }

        function getChartData(property, byPct) {

            var results = [].concat(data);

            filters.forEach(function (filter) {
                results = results.filter(filter.fn);
            })

            var values = results.map(function (item) {
                return item[property];
            })

            var distinctCounts = values.reduce(function (prev, next) {
                var key = encodeKey(next.toString());
                if (!prev[key])
                    prev[key] = 1;
                else
                    prev[key] += 1;

                return prev;
            }, {});


            var ret = []
            for (key in distinctCounts) {
                ret.push([decodeKey(key), distinctCounts[key]])
            }
            
            ret.sort(function(a,b){
                if(parseInt(a[0]) < parseInt(b[0]))
                    return -1
                else
                    return 1
            })

            //Convert to % if required
            if(byPct) {
                var answerCount = parseFloat(results.length)
                ret = ret.map(function(item){
                    return [item[0], Math.round((item[1]/answerCount)*100)]
                })
            }

            return ret;
        }

        function getUniqueValuesByProperty(property) {
            return data.reduce(function (sum, next) {
                var val = next[property];
                if (sum.indexOf(val) !== -1)
                    return sum
                sum.push(val)
                return sum
            }, [])
        }

        function addFilter(filter) {
            var existing = filters.find(function (item) {
                return item.id === filter.id
            })
            if (existing)
                existing.fn = filter.fn
            else
                filters.push(filter)

            drawChart()
        }

        function removeFilter(id) {
            var i = -1;
            for (i = 0; i < filters.length; i++) {
                if (filters[i].id === id)
                    break;
            }
            if (i !== -1)
                filters.splice(i, 1)

            drawChart()
        }


        //Dynamically populate dropdowns
        document.querySelectorAll('.filter').forEach(function (select) {
            var options = getUniqueValuesByProperty(select.id)
                .map(function (text) {
                    return '<option value="' + text + '">' + text + '</option>'
                })
            options.unshift("<option value='all'>All</option>")
            options.join('');
            select.innerHTML = options;
        })

        //Google charts stuff
        google.charts.load("current", {
            packages: ["corechart"]
        });
        google.charts.setOnLoadCallback(drawChart);

        function drawChart() {
            var chartHoverLabel = chartTypePct ? 'Percent' : 'Frequency'
            var data = new google.visualization.DataTable();
            data.addColumn('string', 'Answer');
            data.addColumn('number', chartHoverLabel);
            data.addRows(getChartData(prop, chartTypePct));

            var chartTitle = chartTitles[prop] || "Frequency: " + prop
            chartTitle += chartTypePct ? " (%)" : " (Count)"
            var options = {
                title: chartTitle,
                subtitle: 'More custom text', //Add subtitle text that changes based on selected dimension
                width: "100%",
                height: "100%",
                legend: 'none',
                colors: ['#2b8cbe'],
                bar: {
                    groupWidth: '75%'
                },
                vAxis: {
                    format: 'short'
                }
            };

            var chart = new google.visualization.ColumnChart(document.getElementById('chart_div'));
            chart.draw(data, options)
        }


        //Event handlers
        document.getElementById('prop-select').onchange = function (ev) {
            prop = ev.target.value;
            drawChart();
        };

        document.querySelectorAll('.filter').forEach(function (select) {
            select.onchange = function (ev) {
                var val = ev.target.value
                var prop = ev.target.id
                if (val === 'all')
                    removeFilter(prop)
                else
                    addFilter({
                        id: prop,
                        fn: function (item) {
                            return item[prop] === val;
                        }
                    })
                drawChart();
            }
        })

        document.getElementById('chart-type-toggle').onclick = function(ev) {
            chartTypePct = !chartTypePct;
            ev.target.innerText = chartTypePct ? "Chart by Count" : "Chart by Percent"
            drawChart();
        }