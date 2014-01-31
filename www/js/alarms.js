/**
 * Created by tonyhobbins on 6/01/2014.
 */


$(document).ready(function () {
    getAlarmData();
});


function buildMenu(links) {
    "use strict";

    var ul = $('<ul/>').addClass("X-MENU");

    $.each(links, function(index,linkvals) {
        "use strict";
        var li=$('<li/>')
                .append($('<button/>')
                .text(linkvals.rel)
                .addClass(linkvals.rel))
                .click(function(){
                    postCommand(linkvals.href,"");
                });
        ul.append(li);
    })

    return $('<div/>').addClass("X-CONTROL").append(ul);
}

function getAlarmData() {
    "use strict";
    $.ajax({
            type: 'GET',
            url: '/alarms',
            dataType: 'json'
        }
    ).done(function(payload) {
            load(payload);
        }
    );
}

function postCommand(Path,Body) {
    "use strict";

    $.blockUI({
            message: 'wait',
            css: {
                border: 'none',
                padding: '15px',
                backgroundColor: '#000',
                '-webkit-border-radius': '10px',
                '-moz-border-radius': '10px',
                opacity: .5,
                color: '#fff'
            } });
    $.ajax({
            type: "POST",
            url: Path,
            data: Body,
            contentType: "application/json"
        }
    )
        .done()
        .always(function () {
            setTimeout(function(){$.unblockUI();getAlarmData()},200);
        });

//    getAlarmData();
//    setTimeout(function(){getAlarmData()},2000);
}

function load(payload) {
    "use strict";
    var ts = payload.meta.timestamp;
    var as = payload.data.alarmdata.alarmstate;
    var al = payload.data.alarmdata.links;
    var ps = payload.data.alarmdata.portstatus;

    var portArray=[];

    var alarmDetail = $('<div/>')
        .addClass("X-CONTENT");

    var alarmDiv=$('<ul/>').attr("id",'X-ALARM-LIST');

    $.each(ps, function(index,portstatus) {
        "use strict";
        var p=$('<li/>')
            .append($('<button/>')
            .text(portstatus.description)
            .removeClass()
            .addClass("STATE-" + portstatus.state)
            .addClass("MASK-" + portstatus.mask))
            .click(function(){
                hideAllPorts();
                showPort(portstatus.port);
            });

        var portMenu=buildMenu(portstatus.links);
        var portDetail=getPortDetail(portstatus);

        var portPage=$('<div/>')
            .attr("id","PORT-" + portstatus.port)
            .addClass("twelve columns")
            .addClass("X-PORT-PAGE")
            .addClass("X-PAGE")
            .append(portMenu)
            .append(portDetail);

        portArray.push(portPage);
        alarmDiv.append(p);
    });

    alarmDetail.append(alarmDiv);

    // render the HEADER
    $('#status').html($('<h1/>').text(as));
    $('#status').removeClass().addClass("ALARMSTATE-" + as);
    $('#status').append($('<hr/>'));

    // add the CONTROL (MENU)
    var alarmPage=$('<div/>')
        .attr("id","ALARMS")
        .addClass("twelve columns")
        .addClass("X-ALARM-PAGE")
        .addClass("X-PAGE")
        .append(buildMenu(al))
        .append(alarmDetail);

    $('#main').html(alarmPage);
    portArray.forEach(appendPort);
    hideAllPorts();

}

function appendPort(port) {
    "use strict";
    $('#main').append(port);
}

function getPortDetail(port) {
    // returns a DL containing the Port information
    "use strict";

    var pd=$('<div/>')
        .addClass("X-CONTENT");

    var dl=$('<dl/>')
        .addClass("X-PORT-DETAIL");

    dl.append($('<dt/>').text("Port"));
    dl.append($('<dd/>').text(port.port));
    dl.append($('<dt/>').text("Description"));
    dl.append($('<dd/>').text(port.description));
    dl.append($('<dt/>').text("State"));
    dl.append($('<dd/>').text(port.state));
    dl.append($('<dt/>').text("Mask"));
    dl.append($('<dd/>').text(port.mask));

    return pd.append(dl);
}


function hideAllPorts() {
    "use strict";
    $('.X-ALARM-PAGE').show();
    $('.X-PORT-PAGE').hide();
    $('#footer').empty();

}

function showPort(PortNum) {
    "use strict";
    $('.X-ALARM-PAGE').hide();
    $('#PORT-'+PortNum).show();
    var content=$('<button/>').text("BACK")
        .click(function(){
            hideAllPorts();
        });
    $('#footer').html(content);
}

