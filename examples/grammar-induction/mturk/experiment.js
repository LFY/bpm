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
    n: 0,
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
            entry.push($("#example"+lastTask).html());
            entry.push($("#condition"+lastTask).html());
            entry.push($("#task"+lastTask).html());
            entry.push($("input:radio[name=r_"+lastTask+"]:checked").val());
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