define(["exports", "fable-core"], function (exports, _fableCore) {
    "use strict";

    exports.__esModule = true;
    exports.x = exports.melody = exports.cMinor = exports.cMajor = exports.rythm = exports.scaleSettings = exports.ScaleSettings = exports.random = undefined;
    exports.pickRandom = pickRandom;
    exports.normalDistribution = normalDistribution;
    exports.sigmoid = sigmoid;
    exports.toneReturnCoef = toneReturnCoef;
    exports.getMelody = getMelody;
    exports.getRythm = getRythm;
    exports.convertTone = convertTone;

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    var random = exports.random = {};

    var ScaleSettings = exports.ScaleSettings = function () {
        function ScaleSettings(intervalWeights, toneWeights) {
            _classCallCheck(this, ScaleSettings);

            this.IntervalWeights = intervalWeights;
            this.ToneWeights = toneWeights;
        }

        ScaleSettings.prototype.Equals = function Equals(other) {
            return _fableCore.Util.equalsRecords(this, other);
        };

        ScaleSettings.prototype.CompareTo = function CompareTo(other) {
            return _fableCore.Util.compareRecords(this, other);
        };

        return ScaleSettings;
    }();

    _fableCore.Util.setInterfaces(ScaleSettings.prototype, ["FSharpRecord", "System.IEquatable", "System.IComparable"], "GenSoundCore.ScaleSettings");

    function pickRandom(weights, elements) {
        var totalW = _fableCore.Seq.sum(weights);

        var num = Math.random() * totalW;
        return _fableCore.Seq.find(function (tupledArg) {
            return tupledArg[1] > num;
        }, function (source2) {
            return _fableCore.Seq.zip(elements, source2);
        }(_fableCore.Seq.skip(1, _fableCore.Seq.scan(function (x, y) {
            return x + y;
        }, 0, weights))))[0];
    }

    function normalDistribution(__1, _, x) {
        return Math.pow(2 * Math.pow(_, 2) * 3.141592653589793, -0.5) * Math.pow(2.718281828459045, -(Math.pow(x - __1, 2) / (2 * Math.pow(_, 2))));
    }

    function sigmoid(value) {
        return 1 / (1 + Math.exp(-value));
    }

    function toneReturnCoef(x) {
        return 1 - (1 < Math.pow(0 > x - 0.4 ? 0 : x - 0.4, 2) ? 1 : Math.pow(0 > x - 0.4 ? 0 : x - 0.4, 2));
    }

    function getMelody(rythm, scaleSettings, toneRange) {
        var chooseTone = function chooseTone(previous) {
            return function (toneImportance) {
                return function (jumpBar) {
                    var possibleTones = _fableCore.Seq.toList(_fableCore.Seq.range(-scaleSettings.IntervalWeights.length + 1 + previous, scaleSettings.IntervalWeights.length - 1 + previous));

                    var allIntervalWeights = _fableCore.Seq.map(function (tone) {
                        var interval = Math.abs(tone - previous);
                        return scaleSettings.IntervalWeights[interval];
                    }, possibleTones);

                    var allToneWeights = _fableCore.Seq.map(function (i) {
                        return scaleSettings.ToneWeights[(i % scaleSettings.ToneWeights.length + scaleSettings.ToneWeights.length) % scaleSettings.ToneWeights.length];
                    }, possibleTones);

                    var totalWeights = Float64Array.from(_fableCore.Seq.mapIndexed(function (i, weight) {
                        var tone = previous - _fableCore.Seq.item(i, possibleTones);

                        if (jumpBar >= Math.abs(tone)) {
                            return weight;
                        } else {
                            return weight / Math.exp(1 + 0.1 * (Math.abs(tone) - jumpBar));
                        }
                    }, _fableCore.Seq.mapIndexed(function (i, weight) {
                        var tone = _fableCore.Seq.item(i, possibleTones);

                        if (Math.abs(tone) < Math.abs(previous)) {
                            return weight;
                        } else {
                            return weight * toneReturnCoef(Math.abs(tone) / toneRange);
                        }
                    }, _fableCore.Seq.map2(function (x, y) {
                        return x * y;
                    }, allIntervalWeights, _fableCore.Seq.map(function (y) {
                        return Math.pow(toneImportance, y);
                    }, allToneWeights)))));
                    var resultTone = pickRandom(totalWeights, possibleTones);
                    return resultTone;
                };
            };
        };

        var startingTone = pickRandom(_fableCore.List.ofArray([1, 1, 1]), _fableCore.List.ofArray([0, 2, 4]));
        return _fableCore.Seq.map(function (tuple) {
            return tuple[1];
        }, _fableCore.Seq.scan(function (tupledArg, toneLength) {
            var next = chooseTone(tupledArg[1])(1 + toneLength)(tupledArg[0]);
            return [tupledArg[0] - Math.abs(next - tupledArg[1]) + 4, next];
        }, [2, startingTone], rythm));
    }

    function getRythm(divCoef, recursionCoef, length) {
        var getTactRythm = function getTactRythm(divCoef_1) {
            return function (recursionCoef_1) {
                return Math.random() > divCoef_1 ? _fableCore.List.ofArray([1]) : _fableCore.List.map(function (l) {
                    return l / 2;
                }, _fableCore.List.collect(function (x) {
                    return x;
                }, _fableCore.List.initialize(2, function (i) {
                    return getTactRythm(divCoef_1 / recursionCoef_1)(recursionCoef_1);
                })));
            };
        };

        return _fableCore.List.collect(function (x) {
            return x;
        }, _fableCore.List.initialize(length, function (i) {
            return getTactRythm(divCoef)(recursionCoef);
        }));
    }

    function convertTone(key, tone) {
        return key[(tone % key.length + key.length) % key.length] + 12 * ~~(tone / key.length);
    }

    var scaleSettings = exports.scaleSettings = new ScaleSettings(new Float64Array([1, 5, 5, 3, 3, 0.7, 0.7, 1]), new Float64Array([1.7, 1.5, 1.6, 1, 1.6, 1.5, 1]));
    var rythm = exports.rythm = getRythm(4, 2.9, 3);
    var cMajor = exports.cMajor = new Int32Array([0, 2, 4, 5, 7, 9, 11]);
    var cMinor = exports.cMinor = new Int32Array([0, 2, 3, 5, 7, 8, 10]);
    var melody = exports.melody = Int32Array.from(getMelody(rythm, scaleSettings, 20));
    var x = exports.x = 2 > 1 ? 2 : 1;
});
//# sourceMappingURL=GenSoundCore.js.map