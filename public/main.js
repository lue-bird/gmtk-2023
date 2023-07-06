//////////////////////////////////////////////////////////////////////
//
// PortFunnel.js
// JavaScript runtime code for billstclair/elm-port-funnel
// Copyright (c) 2018-2019 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////
//
// PortFunnel is the single global variable defined by this file.
// It is an object with a `subscribe` property, a function, called as:
//
//   PortFunnel.subscribe
//     (app, {portnames: ['cmdPort', 'subPort']
//           });
//
// The `portnames` property is optional. If included, its value should
// be a two-element array containing the name of the `Cmd` and `Sub`
// ports in `app`. They default as specified above.
//
// The `modules` property is a list of strings, each of which should
// correspond to the 'moduleName' set by one of your PortFunnel-aware
// JavaScript files.
//
// When each `module` JavaScript file is loaded.  It should set
// `PortFunnel.modules['moduleName']`, as illustrated in
// `PortFunnel/Geolocation.js`,so that it can be hooked in to the
// funnelling mechanism below.
//
//////////////////////////////////////////////////////////////////////

(function (scope) {

    PortFunnel = {};
    scope.PortFunnel = PortFunnel;

    PortFunnel.subscribe = subscribe; // called by HTML file
    PortFunnel.modules = {};          // modules[funnelName].cmd set by module JS.
    PortFunnel.sub = null;          // set below

    function subscribe(app, args) {
        if (!args) args = {};
        portNames = args.portNames;
        if (!portNames) {
            portNames = ['cmdPort', 'subPort'];
        }

        var ports = app.ports;
        var sub = ports[portNames[1]];
        PortFunnel.sub = sub;

        var cmd = ports[portNames[0]];
        cmd.subscribe(function (command) {
            var returnValue = commandDispatch(command);
            if (returnValue) {
                sub.send(returnValue);
            }
        });
    }

    // command is of the form:
    //    { module: 'moduleName',
    //      tag: 'command name for module',
    //      args: {name: value, ...}
    //    }
    function commandDispatch(command) {
        if (typeof (command) == 'object') {
            var moduleName = command.module;
            var module = PortFunnel.modules[moduleName];
            if (module) {
                var cmd = module.cmd;
                if (cmd && !queue[moduleName]) {
                    var tag = command.tag;
                    var args = command.args;
                    return cmd(tag, args);
                } else {
                    var list = queue[moduleName];
                    if (!list) list = [];
                    list.push(command);
                    queue[moduleName] = list;
                    if (!queueDrainOutstanding) {
                        scheduleQueueDrain();
                    }
                }
            }
        }
    }

    // queue[moduleName] = an array of commands passed to commandDispatch
    // before the JavaScript module was installed.
    var queue = {};
    var queueDrainOutstanding = false;

    function scheduleQueueDrain() {
        queueDrainOutStanding = true;
        setTimeout(drainQueue, 10);  // is 0.01 second too short?
    }

    function drainQueue() {
        needReschedule = false;
        for (var moduleName in queue) {
            var module = PortFunnel.modules[moduleName];
            if (!module) {
                // Can't happen, but handle it anyway
                delete queue[moduleName];
            } else {
                if (!module.cmd) {
                    needReschedule = true;
                } else {
                    var list = queue[moduleName];
                    delete queue[moduleName];
                    for (var i in list) {
                        var command = list[i];
                        var returnValue = commandDispatch(command);
                        if (returnValue) {
                            PortFunnel.sub.send(returnValue);
                        }
                    }
                }
                if (needReschedule) {
                    scheduleQueueDrain();
                } else {
                    queueDrainOutstanding = false;
                }
            }
        }
    }

}(this))

    ///////////////

    //////////////////////////////////////////////////////////////////////
    //
    // Geolocation.js
    // JavaScript runtime code for Elm PortFunnel.Geolocation module.
    // Copyright (c) 2018-2019 Bill St. Clair <billstclair@gmail.com>
    // Portions Copyright (c) 2015-2016, Evan Czaplicki
    // All rights reserved.
    // Distributed under the BSD-3-Clause License
    // See LICENSE
    //
    //////////////////////////////////////////////////////////////////////

    (function (scope) {
        var moduleName = 'Geolocation';
        var sub;

        function init() {
            var PortFunnel = scope.PortFunnel;
            if (!PortFunnel || !PortFunnel.sub || !PortFunnel.modules) {
                // Loop until PortFunnel.js has initialized itself.
                setTimeout(init, 10);
                return;
            }

            sub = PortFunnel.sub;
            PortFunnel.modules[moduleName] = { cmd: dispatcher };

            // Let the Elm code know we've started
            sub.send({
                module: moduleName,
                tag: "startup",
                args: null
            });
        }
        init();

        function sendObject(tag, args) {
            sub.send({
                module: moduleName,
                tag: tag,
                args: args
            });
        }


        // Elm command dispatching

        var tagTable =
        {
            getlocation: getLocation,
            sendchanges: sendChanges,
            stopchanges: stopChanges
        }

        function dispatcher(tag, args) {
            let f = tagTable[tag];
            if (f) {
                return f(args);
            }
        }

        function getLocation(args) {
            function onError(rawError) {
                var err = encodeError(rawError);
                sendObject("error", err);
            }
            var options = args;
            navigator.geolocation.getCurrentPosition(
                sendPosition, onError, rawOptions(options));
        }

        var watching = false;
        var watchid = null;

        function sendChanges(args) {
            if (!watching) {
                watchid = navigator.geolocation.watchPosition(sendPosition);
                watching = true;
            }
        }

        function stopChanges(args) {
            if (watching) {
                watching = false;
                navigator.geolocation.clearWatch(watchid);
            }
        }


        // Send a position through the subscription port

        function sendPosition(rawPosition) {
            var location = encodeLocation(rawPosition);
            sendObject("location", location);
        }


        // OPTIONS

        var defaultOptions =
        {
            enableHighAccuracy: false,
            timeout: undefined,
            maximumAge: 0
        }

        function rawOptions(options) {
            if (!options) {
                // For debugging. The Elm code always passes options.
                return defaultOptions;
            } else {
                return {
                    enableHighAccuracy: options.enableHighAccuracy,
                    timeout: options.timeout || undefined,
                    maximumAge: options.maximumAge || 0
                };
            }
        }


        // LOCATIONS

        function encodeLocation(rawPosition) {
            var coords = rawPosition.coords;

            var rawAltitude = coords.altitude;
            var rawAccuracy = coords.altitudeAccuracy;
            var altitude =
                (rawAltitude === null || rawAccuracy === null)
                    ? null
                    : {
                        value: rawAltitude,
                        accuracy: rawAccuracy
                    };
            var heading = coords.heading;
            var speed = coords.speed;
            var movement =
                (heading === null || speed === null)
                    ? null
                    : (speed === 0
                        ? 'static'
                        : { speed: speed, degreesFromNorth: heading }
                    );
            return {
                latitude: coords.latitude,
                longitude: coords.longitude,
                accuracy: coords.accuracy,
                altitude: altitude,
                movement: movement,
                timestamp: rawPosition.timestamp
            };
        }


        // ERRORS

        var errorTypes = ['PermissionDenied', 'PositionUnavailable', 'Timeout'];

        function encodeError(rawError) {
            var key = errorTypes[rawError.code - 1];
            var res = {};
            res[key] = rawError.message
            return res;
        }

    })(this);

//////////////


// Initial data passed to Elm (should match `Flags` defined in `Shared.elm`)
// https://guide.elm-lang.org/interop/flags.html
var flags = null

// Start our Elm application
var app = Elm.Main.init({ flags: flags })

// These are the defaults, so you don't need to pass them.
// If you need to use something different, they can be passed
// as the 'portNames' and 'moduleDirectory' properties of
// the second parameter to PortFunnel.subscribe() below.
//     var portNames = ['cmdPort', 'subPort'];
//     var moduleDirectory = 'js/PortFunnel';
var modules =
    ['Geolocation'];

PortFunnel.subscribe(app, { modules: modules });
