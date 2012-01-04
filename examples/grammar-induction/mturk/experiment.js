function showSlide(id) {
    $(".slide").hide();
    $("#"+id).show();
}

function random(a,b) {
    if (typeof b == "undefined") {
	a = a || 2;
	return Math.floor(Math.random()*a);}
    else {
	return Math.floor(Math.random()*(b-a+1)) + a;}
}

Array.prototype.random = function() {
    return this[random(this.length)];
}

showSlide("instructions");

var experiment = {
    n: 1,
    numTasks: 1,
    data: [],
    end: function() {
	showSlide("finished");
	setTimeout(function() {turk.submit(experiment) }, 1500);
    },

    next: function() {
        if (experiment.n>1) {
            var lastTask = experiment.n-1;
            var entry=[];
            entry.push($("#category"+lastTask).html());
            entry.push($("#trainingExamples"+lastTask).html());
            entry.push($("#experimentExamples"+lastTask).html());
            var numChoices = $("#experimentExamples"+lastTask).html().split("\n").length;

            for (var i=1; i<=numChoices; i++) {
                var answer = []; 
                answer.push($("input:radio[name="+lastTask+"-"+i+"-in]:checked").val());
                answer.push($("input:radio[name="+lastTask+"-"+i+"-distinct]:checked").val());
                entry.push(answer);
            }
           
           experiment.data.push(entry);
        }
        
        if (experiment.n>experiment.numTasks) { return experiment.end(); }

        $('html, body').animate({ scrollTop: 0 }, 0);
        $("#currTaskNum").html(experiment.n);
        showSlide("stage"+experiment.n);
        
         
        experiment.n = experiment.n+1;
        experiment.numTasks = parseInt($("#numTasks").html());
    }
}