define(["exports", "fable-core", "./GenSoundCore"], function (exports, _fableCore, _GenSoundCore) {
    "use strict";

    exports.__esModule = true;
    exports.notes = exports.SynthInfo = undefined;
    exports.playStream = playStream;

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    var SynthInfo = exports.SynthInfo = function () {
        function SynthInfo(name) {
            _classCallCheck(this, SynthInfo);

            this.name = name;
        }

        SynthInfo.prototype.Equals = function Equals(other) {
            return _fableCore.Util.equalsRecords(this, other);
        };

        SynthInfo.prototype.CompareTo = function CompareTo(other) {
            return _fableCore.Util.compareRecords(this, other);
        };

        return SynthInfo;
    }();

    _fableCore.Util.setInterfaces(SynthInfo.prototype, ["FSharpRecord", "System.IEquatable", "System.IComparable"], "JsPlayer.SynthInfo");

    function playStream(notes, speed) {
        var convertNote = function convertNote(num) {
            var names = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
            return [names[(num % names.length + names.length) % names.length], ~~(num / names.length) + 3];
        };

        var times = _fableCore.Seq.scan(function (x, y) {
            return x + y;
        }, 1, _fableCore.Seq.map(function (tuple) {
            return tuple[0];
        }, notes));

        var inputSequence = function (source2) {
            return _fableCore.Seq.zip(times, source2);
        }(notes);

        for (var _iterator = inputSequence, _isArray = Array.isArray(_iterator), _i = 0, _iterator = _isArray ? _iterator : _iterator[Symbol.iterator]();;) {
            var _ref;

            if (_isArray) {
                if (_i >= _iterator.length) break;
                _ref = _iterator[_i++];
            } else {
                _i = _iterator.next();
                if (_i.done) break;
                _ref = _i.value;
            }

            var forLoopVar = _ref;
            var note = forLoopVar[1][1];
            var length = forLoopVar[1][0];
            MIDI.noteOn(0, note + 48, 128, speed * forLoopVar[0]);
            MIDI.noteOff(0, note + 48, speed * (forLoopVar[0] + length));
        }
    }

    console.log(_GenSoundCore.melody);

    var notes = exports.notes = function (source2) {
        return _fableCore.Seq.zip(_GenSoundCore.rythm, source2);
    }(_fableCore.Seq.map(function (tone) {
        return (0, _GenSoundCore.convertTone)(_GenSoundCore.cMinor, tone);
    }, _GenSoundCore.melody));

    MIDI.loadPlugin({
        instrument: "acoustic_grand_piano",
        onsuccess: function onsuccess() {
            console.log("piano loaded");
            playStream(notes, 2);
        }
    });
});
//# sourceMappingURL=JsPlayer.js.map