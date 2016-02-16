window.addEventListener('load',function(event){

    getPosixTime(function(posixtime){
	var elem = document.getElementById("posixtime");
	elem.innerHTML = formatpt(posixtime);
    });

    
    function formatpt (posixtime) {
	console.log(posixtime)
	var dt = new Date(posixtime);
	return dt.toString();
    }

    
    function getPosixTime(fOnResponse) {
	
	var xhr = new XMLHttpRequest();
	xhr.open('GET', '/posixtime', true);

	xhr.onreadystatechange = function(event) {
	    if (this.readyState == 4 && this.status == 200) {
		var posixtime = parseInt(this.responseText,10);
		fOnResponse(posixtime);
	    }
	};
	
	xhr.send();    
    }

    
},false);
