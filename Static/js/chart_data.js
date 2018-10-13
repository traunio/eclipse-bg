$("#myChart").hide();
$(document).ready(function() {

     var ctx = document.getElementById("myChart");
     var myChart = new Chart(ctx, {
	 type: 'horizontalBar',
	 data: {labels: [], datasets:[{label: "Tulokset", backgroundColor: 'rgb(255, 99, 132)',
				       data: []}]},
	 options: {
	     scales: {
		 xAxes: [{
		     stacked: false,
		     scaleLabel: {
			 display: true,
			 labelString: 'Probability of outcome [%]'}
		 }],
		 yAxes: [{
		     stacked: false,
		     ticks: { beginAtZero:true   },
		     scaleLabel: {
			 display: true,
			 labelString: 'Number of damages' }
		 }]
	     }
	 } });

    function updateChart() {
	var postis = {ships: [[[$("#intercept_1").val()
				, $("#intercept_2").val()
				, $("#intercept_3").val()
				, $("#intercept_4").val()],
			       Number($("#intercept_fleet").val())],
			      [[$("#cruiser_1").val()
				, $("#cruiser_2").val()
				, $("#cruiser_3").val()
				, $("#cruiser_4").val()
  			        , $("#cruiser_5").val()
			        , $("#cruiser_6").val()],
			       Number($("#cruiser_fleet").val())],
			      [[$("#dread_1").val()
				, $("#dread_2").val()
				, $("#dread_3").val()
				, $("#dread_4").val()
  			        , $("#dread_5").val()
			        , $("#dread_6").val()
				, $("#dread_7").val()
				, $("#dread_8").val()],
		      Number($("#dread_fleet").val())],
			      [[$("#starbase_1").val()
				, $("#starbase_2").val()
				, $("#starbase_3").val()
				, $("#starbase_4").val()
  			        , $("#starbase_5").val()],
			       Number($("#starbase_fleet").val())]],
		      enemyShield: Number($("#enemy-shields").val())};
	jQuery.post("/data", JSON.stringify(postis), function(data) {
	    if(data.labels && data.probs && data.mprobs) {
		if (data.labels.length > 1) {
		    $("#myChart").show();
		} else {
		    $("#myChart").hide();
		}
		 myChart.data.datasets = [{label: "Cannons",
					   backgroundColor: 'rgb(255, 99, 132)',
					   data: data.probs},
					  {label: "Missiles",
					   backgroundColor: 'rgb(208, 182, 234)',
					   data: data.mprobs}];
		 myChart.data.labels = data.labels;
		 myChart.update();
	     }
	     else {console.log("Something went wrong"); }

	 });

    }

    $('select').on('change', function() {
	updateChart();
    });

    // $("#int_inc_ions").click(function (){
    // 	var temp = Number($("#intercept_ions").text());
    // 	if (temp < 4) {
    // 	    temp += 1;
    // 	}
    // 	$("#intercept_ions").text(temp.toString());
    // });

    // $("#int_dec_ions").click(function (){
    // 	var temp = Number($("#intercept_ions").text());
    // 	if (temp > 0) {
    // 	    temp -= 1;
    // 	}
    // 	$("#intercept_ions").text(temp.toString());
    // });

    
});

			      

	 
