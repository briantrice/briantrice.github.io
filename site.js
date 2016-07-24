if(document.getElementById)
    document.write('<style type="text/css"> div { display: none; } div#navigation { display: block; } </style>');

function toggleArea(id) {
    var elem = document.getElementById(id);
    if (elem)
	elem.style.display = (elem.style.display == 'block') ? 'none' : 'block';
    else
	throw ("No element with id: " + id);
}

function handleNavLinkClick (event) {
    if (!event) event = window.event;
    var sectionLink = (event.target) ? event.target : event.srcElement;
    var sectionName = sectionLink.firstChild.nodeValue;
    if (!document.getElementById(sectionName))
	throw("No section corresponding with navigation link " + sectionName);
    var activeSectionLink = document.getElementById('active');
    if (!activeSectionLink)
	throw("No active section.");
    activeSectionLink.id = null;
    sectionLink.id ='active';
    refreshSections();
}

function initializeNavigation () {
    if (!document.getElementById || !document.getElementsByTagName) return;
    // Adds section navigation links and behavior.
    var divs = document.getElementsByTagName('div');
    var navlist = document.createElement('ul');
    var navLinks = new Array();
    for (var k=0; k<divs.length; k++) {
	if (divs[k].className == 'section') {
	    var navLink = document.createElement('a');
	    var navLinkItem = document.createElement('li');
	    navLink.href = '#';
	    navLink.innerHTML = divs[k].id;
	    navLink.title = navLink.innerHTML;
	    navLink.onclick = handleNavLinkClick;
	    navLinks.push(navLink);
	    navLinkItem.appendChild(navLink);
	    navlist.appendChild(navLinkItem);
	}
    }
    navLinks[0].id = "active"; // Activate the first section
    document.getElementById('navigation').appendChild(navlist);
    // Adds behavior to page sections for navigation.
    for (var j=0;j<divs.length;j++) {
	var div = divs[j];
	if (div.id) {
	    if (div.className != 'x') {
		div.style.display = 'block';
		for (var i=0;i<navLinks.length;i++)
		    if (div.id == navLinks[i].firstChild.nodeValue &&
			navLinks[i].id != 'active')
			div.style.display = 'none';
	    }
	} else
	    div.style.display = 'block';
    }
    // Adds outlining behavior for expansion
    // Find elements with 'outline' class
    // For each, aggregate following elements up to the next with same tag
    // Replace those elements with a div and set up the links for expand/contract
    // Adds behavior to headings for expansion.
    var links = document.getElementsByTagName('a');
    for (var i=0;i<links.length;i++) {
	var link = links[i];
	if (link.className == 'xp') {
	    var targetID = link.href.split("#")[1];
	    var targetArea = document.getElementById(targetID);
	    if (targetArea) {
		link.href = "javascript:toggleArea('"+targetID+"');";
		link.innerHTML = link.innerHTML + '&raquo;';
		link.style.textDecoration = 'none';
		// Adds a matching close-link at the end of the content.
		var closeLink = document.createElement('a');
		closeLink.href = link.href;
		closeLink.innerHTML = '&laquo;';
		closeLink.style.textDecoration = 'none';
		closeLink.title = link.title;
		targetArea.appendChild(closeLink);
	    }
	}
    }
    refreshSections();
    return true;
}

window.onload = initializeNavigation;

// Refreshes the sections according to navigation state.
function refreshSections () {
    var nav = document.getElementById('navigation');
    var activeSectionLink = document.getElementById('active');
    if (!activeSectionLink)
	throw("No active section.");
    var links = nav.getElementsByTagName('a');
    for (var i=0;i<links.length;i++) {
	var link = links[i];
	var sectionName = link.firstChild.nodeValue;
	var section = document.getElementById(sectionName);
	if (!section)
	    throw("No section for navigation link "+sectionName);
	section.style.display = (link.id == 'active') ? 'block' : 'none';
    }
    return true;
}
