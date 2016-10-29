define(["./GenSoundCore", "fable-core"], function (_GenSoundCore, _fableCore) {
    "use strict";

    (function main(argv) {
        _GenSoundCore.melody;

        _fableCore.String.fsFormat("%A")(function (x) {
            console.log(x);
        })(argv);

        return 0;
    })(process.argv.slice(2));
});
//# sourceMappingURL=Program.js.map