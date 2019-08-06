S.log("loading...");

var D = {
    left: "left",
    right: "right",
    top : "top",
    bottom : "bottom"
};

function seq(...args) {
    return S.op("sequence", {"operations": args});
}

function push(dir, mod) {
    let axis = (dir === D.left || dir === D.right) ? "X" : "Y";
    return S.op("push", {
        "direction" : `${dir}`,
        "style" : `bar-resize:screenSize${axis}${mod}`
    });
}

function throwTo(screen, wmod, hmod) {
    if (typeof wmod === "undefined") {
        wmod = "";
    }
    if (typeof hmod === "undefined") {
        hmod = "";
    }
    return seq(
        S.op("throw", {
            "screen" : `${screen}`,
            "width" : `screenSizeX${wmod}`,
            "height" : `screenSizeY${hmod}`
        }),
        mouseToScreen(screen));
}

function throwToHalf(screen, dir) {
    return seq(
        throwTo(screen, "/2"),
        push(dir, "/2"),
        mouseToScreen(screen, dir == D.left ? "*0.25" : "*0.75"),
    );
}

function mouseToScreen(screen, xmod, ymod) {
    if (typeof xmod === "undefined") {
        xmod = "/2";
    }
    if (typeof ymod === "undefined") {
        ymod = "/2";
    }
    return S.op("mouse", {
        "x": `screenOriginX+screenSizeX${xmod}`,
        "y": `screenOriginY+screenSizeY${ymod}`,
        "screen": `${screen}`,
    });
}

S.bindAll({
    "backslash:backslash,ctrl": push(D.left, ""),

    "h:backslash,ctrl": push(D.left, "-8"),
    "l:backslash,ctrl": push(D.right, "-8"),

    "j:backslash,ctrl": push(D.bottom, "/2"),
    "k:backslash,ctrl": push(D.top, "/2"),
    "[:backslash,ctrl": push(D.left, "/2"),
    "]:backslash,ctrl": push(D.right, "/2"),

    "pad1:ctrl" : throwTo(0),
    "pad2:ctrl" : throwTo(1),
    "pad3:ctrl" : throwToHalf(2, D.left),
    "pad4:ctrl" : throwToHalf(2, D.right),
    "pad0:ctrl" : throwTo(3),

    "1:backslash,ctrl" : throwTo(0),
    "2:backslash,ctrl" : throwTo(1),
    "3:backslash,ctrl" : throwToHalf(2, D.left),
    "4:backslash,ctrl" : throwToHalf(2, D.right),
    "0:backslash,ctrl" : throwTo(3),

    "pad9:ctrl": mouseToScreen(3),
});

S.log("loaded");
