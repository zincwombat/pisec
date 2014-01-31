/**
 * Created by tonyhobbins on 6/01/2014.
 */
$(document).ready(function () {
    $.ajax({
        type: 'GET',
        url: 'http://192.168.2.2:8888/alarms',
        dataType: 'json',
        success: function (data) {
            var ts = data.meta.timestamp;
            $('#alarms').html(ts);
        }
    });
});

