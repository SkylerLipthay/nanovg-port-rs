#![allow(non_snake_case)]

#[cfg(target_arch = "wasm32")]
extern crate web_sys;
#[cfg(target_arch = "wasm32")]
#[macro_use] extern crate static_assertions;

macro_rules! reserve {
    ($vec:expr, $len:expr) => {
        {
            let free = $vec.capacity() - $vec.len();
            if $len as usize > free {
                $vec.reserve($len as usize - free);
            }
        }
    };
}

#[cfg(target_arch = "wasm32")]
pub mod webgl;

// TODO:
//
// Skipping text and image operations and types for now. Public:
//
// * `NVGalign`
// * `NVGglyphPosition`
// * `NVGtextRow`
// * `NVGimageFlags`
// * `NVGrenderer#createTexture`
// * `NVGrenderer#deleteTexture`
// * `NVGrenderer#updateTexture`
// * `NVGrenderer#getTextureSize`
// * `NVGrenderer#triangles`
// * `NVGpaint.image`
// * `nvgCreateImage`
// * `nvgCreateImageMem`
// * `nvgCreateImageRGBA`
// * `nvgUpdateImage`
// * `nvgImageSize`
// * `nvgDeleteImage`
// * `nvgImagePattern`
// * `nvgFontSize`
// * `nvgFontBlur`
// * `nvgTextLetterSpacing`
// * `nvgTextLineHeight`
// * `nvgTextAlign`
// * `nvgFontFaceId`
// * `nvgFontFace`
// * `nvgText`
// * `nvgTextBox`
// * `nvgTextGlyphPositions`
// * `nvgTextBreakLines`
// * `nvgTextBounds`
// * `nvgTextBoxBounds`
// * `nvgTextMetrics`
//
// Private:
//
// * `NVG_INIT_FONTIMAGE_SIZE`
// * `NVG_MAX_FONTIMAGE_SIZE`
// * `NVG_MAX_FONTIMAGES`
// * `NVGstate.fontSize`
// * `NVGstate.letterSpacing`
// * `NVGstate.lineHeight`
// * `NVGstate.fontBlur`
// * `NVGstate.textAlign`
// * `NVGstate.fontId`
// * `NVGcontext.fs`
// * `NVGcontext.fontImages`
// * `NVGcontext.fontImageIdx`
// * `nvg__quantize`
// * `nvg__getFontScale`
// * `nvg__flushTextTexture`
// * `nvg__allocTextAtlas`
// * `nvg__renderText`
// * `NVGcodepointType`
//
// Not implementing these items, I think:
//
// * `NVG_INIT_COMMANDS_SIZE` (antiquated)
// * `NVG_INIT_POINTS_SIZE` (antiquated)
// * `NVG_INIT_PATHS_SIZE` (antiquated)
// * `NVG_INIT_VERTS_SIZE` (antiquated)
// * `NVG_MAX_STATES` (antiquated)
// * `NVG_COUNTOF` (antiquated)
// * `NVGcontext.drawCallCount` (this is for debugging)
// * `NVGcontext.fillTriCount` (this is for debugging)
// * `NVGcontext.strokeTriCount` (this is for debugging)
// * `NVGcontext.textTriCount` (this is for debugging)
// * `nvgDebugDumpPathCache` (this is for debugging)
// * `nvgInternalParams` (probably not needed by renderer implementation)
// * `nvgDeleteInternal` (handled by `Drop`)
// * `NVGparams.create` (handled by renderer implementation)
// * `NVGparams.delete` (handled by `Drop`)

use std::ops::{Index, IndexMut};

// START: `nanovg.h`

pub const NVG_PI: f64 = 3.14159265358979323846264338327;

#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub struct NVGcolor {
    pub r: f32,
    pub g: f32,
    pub b: f32,
    pub a: f32,
}

impl Index<usize> for NVGcolor {
    type Output = f32;

    fn index(&self, index: usize) -> &f32 {
        match index {
            0 => &self.r,
            1 => &self.g,
            2 => &self.b,
            3 => &self.g,
            _ => panic!("unsupported index on NVGcolor (must be one of: 0, 1, 2, 3)"),
        }
    }
}

impl IndexMut<usize> for NVGcolor {
    fn index_mut(&mut self, index: usize) -> &mut f32 {
        match index {
            0 => &mut self.r,
            1 => &mut self.g,
            2 => &mut self.b,
            3 => &mut self.g,
            _ => panic!("unsupported index on NVGcolor (must be one of: 0, 1, 2, 3)"),
        }
    }
}

pub type NVGtransform = [f32; 6];

#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct NVGpaint {
    pub xform: NVGtransform,
    pub extent: [f32; 2],
    pub radius: f32,
    pub feather: f32,
    pub innerColor: NVGcolor,
    pub outerColor: NVGcolor,
}

pub type NVGwinding = i32;
pub const NVG_CCW: NVGwinding = 1;
pub const NVG_CW: NVGwinding = 2;

pub type NVGsolidity = i32;
pub const NVG_SOLID: NVGsolidity = 1;
pub const NVG_HOLE: NVGsolidity = 2;

pub type NVGlineCap = i32;
pub const NVG_BUTT: NVGlineCap = 0;
pub const NVG_ROUND: NVGlineCap = 1;
pub const NVG_SQUARE: NVGlineCap = 2;
pub const NVG_BEVEL: NVGlineCap = 3;
pub const NVG_MITER: NVGlineCap = 4;

pub type NVGblendFactor = i32;
pub const NVG_ZERO: NVGblendFactor = 1 << 0;
pub const NVG_ONE: NVGblendFactor = 1 << 1;
pub const NVG_SRC_COLOR: NVGblendFactor = 1 << 2;
pub const NVG_ONE_MINUS_SRC_COLOR: NVGblendFactor = 1 << 3;
pub const NVG_DST_COLOR: NVGblendFactor = 1 << 4;
pub const NVG_ONE_MINUS_DST_COLOR: NVGblendFactor = 1 << 5;
pub const NVG_SRC_ALPHA: NVGblendFactor = 1 << 6;
pub const NVG_ONE_MINUS_SRC_ALPHA: NVGblendFactor = 1 << 7;
pub const NVG_DST_ALPHA: NVGblendFactor = 1 << 8;
pub const NVG_ONE_MINUS_DST_ALPHA: NVGblendFactor = 1 << 9;
pub const NVG_SRC_ALPHA_SATURATE: NVGblendFactor = 1 << 10;

pub type NVGcompositeOperation = i32;
pub const NVG_SOURCE_OVER: NVGcompositeOperation = 0;
pub const NVG_SOURCE_IN: NVGcompositeOperation = 1;
pub const NVG_SOURCE_OUT: NVGcompositeOperation = 2;
pub const NVG_ATOP: NVGcompositeOperation = 3;
pub const NVG_DESTINATION_OVER: NVGcompositeOperation = 4;
pub const NVG_DESTINATION_IN: NVGcompositeOperation = 5;
pub const NVG_DESTINATION_OUT: NVGcompositeOperation = 6;
pub const NVG_DESTINATION_ATOP: NVGcompositeOperation = 7;
pub const NVG_LIGHTER: NVGcompositeOperation = 8;
pub const NVG_COPY: NVGcompositeOperation = 9;
pub const NVG_XOR: NVGcompositeOperation = 10;

#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct NVGcompositeOperationState {
    pub srcRGB: NVGblendFactor,
    pub dstRGB: NVGblendFactor,
    pub srcAlpha: NVGblendFactor,
    pub dstAlpha: NVGblendFactor,
}

pub type NVGtexture = i32;
pub const NVG_TEXTURE_ALPHA: NVGtexture = 1;
pub const NVG_TEXTURE_RGBA: NVGtexture = 2;

#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct NVGscissor {
    pub xform: NVGtransform,
    pub extent: [f32; 2],
}

#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct NVGvertex {
    pub x: f32,
    pub y: f32,
    pub u: f32,
    pub v: f32,
}

#[derive(Clone, Debug)]
#[repr(C)]
pub struct NVGpath {
    pub first: i32,
    pub count: i32,
    pub closed: u8,
    pub nbevel: i32,
    pub fillFirst: usize,
    pub fillCount: i32,
    pub strokeFirst: usize,
    pub strokeCount: i32,
    pub winding: NVGwinding,
    pub convex: i32,
}


pub struct NVGparams {
	pub edgeAntiAlias: i32,
    pub render: Box<NVGrender>,
}

pub trait NVGrender {
    fn viewport(&mut self, width: f32, height: f32, devicePixelRatio: f32);
    fn cancel(&mut self);
    fn flush(&mut self);
    fn fill(&mut self, paint: &NVGpaint, compositeOperation: NVGcompositeOperationState,
            scissor: &NVGscissor, fringe: f32, bounds: [f32; 4], paths: &[NVGpath],
            verts: &[NVGvertex]);
    fn stroke(&mut self, paint: &NVGpaint, compositeOperation: NVGcompositeOperationState,
              scissor: &NVGscissor, fringe: f32, strokeWidth: f32, paths: &[NVGpath],
              verts: &[NVGvertex]);
}

// END: `nanovg.h`

// START: `nanovg.c`

const NVG_KAPPA90: f64 = 0.5522847493;

type NVGpointFlag = i32;
const NVG_PT_CORNER: NVGpointFlag = 1 << 0;
const NVG_PT_LEFT: NVGpointFlag = 1 << 1;
const NVG_PT_BEVEL: NVGpointFlag = 1 << 2;
const NVG_PR_INNERBEVEL: NVGpointFlag = 1 << 3;

#[derive(Clone, Copy, Debug)]
#[repr(C)]
struct NVGstate {
    compositeOperation: NVGcompositeOperationState,
    shapeAntiAlias: i32,
    fill: NVGpaint,
    stroke: NVGpaint,
    strokeWidth: f32,
    miterLimit: f32,
    lineJoin: NVGlineCap,
    lineCap: NVGlineCap,
    alpha: f32,
    xform: NVGtransform,
    scissor: NVGscissor,
}

#[derive(Clone, Copy, Debug)]
#[repr(C)]
struct NVGpoint {
    x: f32,
    y: f32,
    dx: f32,
    dy: f32,
    len: f32,
    dmx: f32,
    dmy: f32,
    flags: NVGpointFlag,
}

#[derive(Clone, Debug)]
struct NVGpathCache {
    points: Vec<NVGpoint>,
    paths: Vec<NVGpath>,
    verts: Vec<NVGvertex>,
    bounds: [f32; 4],
}

pub struct NVGcontext {
    params: NVGparams,
    commands: Vec<NVGcommand>,
    commandx: f32,
    commandy: f32,
    states: Vec<NVGstate>,
    cache: NVGpathCache,
    tessTol: f32,
    distTol: f32,
    fringeWidth: f32,
    devicePxRatio: f32,
}

enum NVGcommand {
    MoveTo {
        x: f32,
        y: f32,
    },
    LineTo {
        x: f32,
        y: f32,
    },
    BezierTo {
        c1x: f32,
        c1y: f32,
        c2x: f32,
        c2y: f32,
        x: f32,
        y: f32,
    },
    Close,
    Winding(NVGwinding),
}

#[inline]
fn nvg__min<T: PartialOrd>(a: T, b: T) -> T {
    if a < b { a } else { b }
}

#[inline]
fn nvg__max<T: PartialOrd>(a: T, b: T) -> T {
    if a > b { a } else { b }
}

#[inline]
fn nvg__clamp<T: PartialOrd>(a: T, min: T, max: T) -> T {
    if a < min { min } else if a > max { max } else { a }
}

#[inline]
fn nvg__cross(dx0: f32, dy0: f32, dx1: f32, dy1: f32) -> f32 {
    dx1 * dy0 - dx0 * dy1
}

#[inline]
fn nvg__hypotenuse(x: f32, y: f32) -> f32 {
    (x * x + y * y).sqrt()
}

fn nvg__normalize(x: f32, y: f32) -> (f32, f32) {
    let d = nvg__hypotenuse(x, y);
    if d > 1.0e-6 {
        let id = 1.0 / d;
        (x * id, y * id)
    } else {
        (x, y)
    }
}

fn nvg__compositeOperationState(op: NVGcompositeOperation) -> NVGcompositeOperationState {
    let (sfactor, dfactor) = match op {
        NVG_SOURCE_OVER => (NVG_ONE, NVG_ONE_MINUS_SRC_ALPHA),
        NVG_SOURCE_IN => (NVG_DST_ALPHA, NVG_ZERO),
        NVG_SOURCE_OUT => (NVG_ONE_MINUS_DST_ALPHA, NVG_ZERO),
        NVG_ATOP => (NVG_DST_ALPHA, NVG_ONE_MINUS_SRC_ALPHA),
        NVG_DESTINATION_OVER => (NVG_ONE_MINUS_DST_ALPHA, NVG_ONE),
        NVG_DESTINATION_IN => (NVG_ZERO, NVG_SRC_ALPHA),
        NVG_DESTINATION_OUT => (NVG_ZERO, NVG_ONE_MINUS_SRC_ALPHA),
        NVG_DESTINATION_ATOP => (NVG_ONE_MINUS_DST_ALPHA, NVG_SRC_ALPHA),
        NVG_LIGHTER => (NVG_ONE, NVG_ONE),
        NVG_XOR => (NVG_ONE_MINUS_DST_ALPHA, NVG_ONE_MINUS_SRC_ALPHA),
        NVG_COPY | _ => (NVG_ONE, NVG_ZERO),
    };

    NVGcompositeOperationState {
        srcRGB: sfactor,
        dstRGB: dfactor,
        srcAlpha: sfactor,
        dstAlpha: dfactor,
    }
}

impl NVGcontext {
    fn setDevicePixelRatio(&mut self, ratio: f32) {
        self.tessTol = 0.25 / ratio;
        self.distTol = 0.01 / ratio;
        self.fringeWidth = 1.0 / ratio;
        self.devicePxRatio = ratio;
    }

    fn getState(&self) -> &NVGstate {
        self.states.last().expect("state stack length unexpectedly short")
    }

    fn getStateMut(&mut self) -> &mut NVGstate {
        self.states.last_mut().expect("state stack length unexpectedly short")
    }
}

impl NVGstate {
    fn new() -> NVGstate {
        NVGstate {
            fill: NVGpaint::color(nvgRGBA(255, 255, 255, 255)),
            stroke: NVGpaint::color(nvgRGBA(0, 0, 0, 255)),
            compositeOperation: nvg__compositeOperationState(NVG_SOURCE_OVER),
            shapeAntiAlias: 1,
            strokeWidth: 1.0,
            miterLimit: 10.0,
            lineCap: NVG_BUTT,
            lineJoin: NVG_MITER,
            alpha: 1.0,
            xform: nvgTransformIdentity(),
            scissor: NVGscissor::new(),
        }
    }
}

impl NVGpaint {
    fn color(c: NVGcolor) -> NVGpaint {
        NVGpaint {
            xform: nvgTransformIdentity(),
            extent: [0.0, 0.0],
            radius: 0.0,
            feather: 1.0,
            innerColor: c,
            outerColor: c,
        }
    }
}

impl NVGscissor {
    fn new() -> NVGscissor {
        NVGscissor {
            xform: [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
            extent: [-1.0, -1.0],
        }
    }
}

impl NVGpathCache {
    fn new() -> NVGpathCache {
        NVGpathCache {
            points: Vec::new(),
            paths: Vec::new(),
            verts: Vec::new(),
            bounds: [0.0, 0.0, 0.0, 0.0],
        }
    }
}

pub fn nvgCreateInternal(params: NVGparams) -> NVGcontext {
    let mut ctx = NVGcontext {
        params,
        commands: Vec::new(),
        commandx: 0.0,
        commandy: 0.0,
        states: vec![NVGstate::new()],
        cache: NVGpathCache::new(),
        tessTol: 0.0,
        distTol: 0.0,
        fringeWidth: 0.0,
        devicePxRatio: 0.0,
    };

    ctx.setDevicePixelRatio(1.0);
    ctx
}

pub fn nvgBeginFrame(
    ctx: &mut NVGcontext,
    windowWidth: f32,
    windowHeight: f32,
    devicePixelRatio: f32,
) {
    ctx.states = vec![NVGstate::new()];
    ctx.setDevicePixelRatio(devicePixelRatio);
    ctx.params.render.viewport(windowWidth, windowHeight, devicePixelRatio);
}

pub fn nvgCancelFrame(ctx: &mut NVGcontext) {
    ctx.params.render.cancel();
}

pub fn nvgEndFrame(ctx: &mut NVGcontext) {
    ctx.params.render.flush();
}

pub fn nvgRGB(r: u8, g: u8, b: u8) -> NVGcolor {
	nvgRGBA(r, g, b, 255)
}

pub fn nvgRGBf(r: f32, g: f32, b: f32) -> NVGcolor {
	nvgRGBAf(r, g, b, 1.0)
}

pub fn nvgRGBA(r: u8, g: u8, b: u8, a: u8) -> NVGcolor {
	NVGcolor {
        r: r as f32 / 255.0,
        g: g as f32 / 255.0,
        b: b as f32 / 255.0,
        a: a as f32 / 255.0,
    }
}

pub fn nvgRGBAf(r: f32, g: f32, b: f32, a: f32) -> NVGcolor {
	NVGcolor { r, g, b, a }
}

pub fn nvgTransRGBA(mut c: NVGcolor, a: u8) -> NVGcolor {
	c.a = a as f32 / 255.0;
	c
}

pub fn nvgTransRGBAf(mut c: NVGcolor, a: f32) -> NVGcolor {
	c.a = a;
	c
}

pub fn nvgLerpRGBA(c0: NVGcolor, c1: NVGcolor, u: f32) -> NVGcolor {
	let mut cint = NVGcolor { r: 0.0, g: 0.0, b: 0.0, a: 0.0 };
	let u = nvg__clamp(u, 0.0, 1.0);
	let oneminu = 1.0 - u;
    for i in 0..4 {
		cint[i] = c0[i] * oneminu + c1[i] * u;
	}
    cint
}

pub fn nvgHSL(h: f32, s: f32, l: f32) -> NVGcolor {
	return nvgHSLA(h, s, l, 255);
}

pub fn nvgHSLA(mut h: f32, mut s: f32, mut l: f32, a: u8) -> NVGcolor {
	h = h % 1.0;
	if h < 0.0 {
        h += 1.0;
    }

	s = nvg__clamp(s, 0.0, 1.0);
	l = nvg__clamp(l, 0.0, 1.0);

	let m2 = if l <= 0.5 { l * (1.0 + s) } else { l + s - l * s };
	let m1 = 2.0 * l - m2;

    NVGcolor {
	    r: nvg__clamp(nvg__hue(h + 1.0 / 3.0, m1, m2), 0.0, 1.0),
	    g: nvg__clamp(nvg__hue(h, m1, m2), 0.0, 1.0),
	    b: nvg__clamp(nvg__hue(h - 1.0 / 3.0, m1, m2), 0.0, 1.0),
	    a: a as f32 / 255.0,
    }
}

fn nvg__hue(mut h: f32, m1: f32, m2: f32) -> f32 {
	if h < 0.0 {
        h += 1.0;
    }

	if h > 1.0 {
        h -= 1.0;
    }

	if h < 1.0 / 6.0 {
		m1 + (m2 - m1) * h * 6.0
    } else if h < 3.0 / 6.0 {
		m2
    } else if h < 4.0 / 6.0 {
		m1 + (m2 - m1) * (2.0 / 3.0 - h) * 6.0
    } else {
        m1
    }
}

pub fn nvgTransformIdentity() -> NVGtransform {
    [1.0, 0.0, 0.0, 1.0, 0.0, 0.0]
}

pub fn nvgTransformTranslate(tx: f32, ty: f32) -> NVGtransform {
    [1.0, 0.0, 0.0, 1.0, tx, ty]
}

pub fn nvgTransformScale(sx: f32, sy: f32) -> NVGtransform {
    [sx, 0.0, 0.0, sy, 0.0, 0.0]
}

pub fn nvgTransformRotate(a: f32) -> NVGtransform {
    let cs = a.cos();
    let sn = a.sin();
    [cs, sn, -sn, cs, 0.0, 0.0]
}

pub fn nvgTransformSkewX(a: f32) -> NVGtransform {
    [1.0, 0.0, a.tan(), 1.0, 0.0, 0.0]
}

pub fn nvgTransformSkewY(a: f32) -> NVGtransform {
    [1.0, a.tan(), 0.0, 1.0, 0.0, 0.0]
}

pub fn nvgTransformMultiply(t: NVGtransform, s: NVGtransform) -> NVGtransform {
	let t0 = t[0] * s[0] + t[1] * s[2];
	let t2 = t[2] * s[0] + t[3] * s[2];
	let t4 = t[4] * s[0] + t[5] * s[2] + s[4];
	let t1 = t[0] * s[1] + t[1] * s[3];
	let t3 = t[2] * s[1] + t[3] * s[3];
	let t5 = t[4] * s[1] + t[5] * s[3] + s[5];
    [t0, t1, t2, t3, t4, t5]
}

pub fn nvgTransformPremultiply(t: NVGtransform, s: NVGtransform) -> NVGtransform {
	nvgTransformMultiply(s, t)
}

pub fn nvgTransformInverse(t: NVGtransform) -> Option<NVGtransform> {
    let det = t[0] as f64 * t[3] as f64 - t[2] as f64 * t[1] as f64;
	if det > -1.0e-6 && det < 1.0e-6 {
		return None;
	}

	let invdet = 1.0 / det;
	let t0 = (t[3] as f64 * invdet) as f32;
	let t2 = (-t[2] as f64 * invdet) as f32;
	let t4 = ((t[2] as f64 * t[5] as f64 - t[3] as f64 * t[4] as f64) * invdet) as f32;
	let t1 = (-t[1] as f64 * invdet) as f32;
	let t3 = (t[0] as f64 * invdet) as f32;
	let t5 = ((t[1] as f64 * t[4] as f64 - t[0] as f64 * t[5] as f64) * invdet) as f32;
    Some([t0, t1, t2, t3, t4, t5])
}

pub fn nvgTransformPoint(t: &NVGtransform, sx: f32, sy: f32) -> (f32, f32) {
	let dx = sx * t[0] + sy * t[2] + t[4];
	let dy = sx * t[1] + sy * t[3] + t[5];
    (dx, dy)
}

pub fn nvgDegToRad(deg: f32) -> f32 {
	return deg / 180.0 * NVG_PI as f32;
}

pub fn nvgRadToDeg(rad: f32) -> f32 {
	return rad / NVG_PI as f32 * 180.0;
}

pub fn nvgSave(ctx: &mut NVGcontext) {
    if !ctx.states.is_empty() {
        let state = ctx.getState().clone();
        ctx.states.push(state);
    } else {
        ctx.states.push(NVGstate::new())
    }
}

pub fn nvgRestore(ctx: &mut NVGcontext) {
    if ctx.states.len() > 1 {
        ctx.states.pop();
    }
}

pub fn nvgReset(ctx: &mut NVGcontext) {
    ctx.states.pop();
    ctx.states.push(NVGstate::new());
}

pub fn nvgShapeAntiAlias(ctx: &mut NVGcontext, enabled: bool) {
	ctx.getStateMut().shapeAntiAlias = if enabled { 1 } else { 0 };
}

pub fn nvgStrokeWidth(ctx: &mut NVGcontext, width: f32) {
	ctx.getStateMut().strokeWidth = width;
}

pub fn nvgMiterLimit(ctx: &mut NVGcontext, limit: f32) {
	ctx.getStateMut().miterLimit = limit;
}

pub fn nvgLineCap(ctx: &mut NVGcontext, cap: NVGlineCap) {
	ctx.getStateMut().lineCap = cap;
}

pub fn nvgLineJoin(ctx: &mut NVGcontext, join: NVGlineCap) {
	ctx.getStateMut().lineJoin = join;
}

pub fn nvgGlobalAlpha(ctx: &mut NVGcontext, alpha: f32) {
	ctx.getStateMut().alpha = alpha;
}

pub fn nvgTransform(ctx: &mut NVGcontext, t: NVGtransform) {
    let state = ctx.getStateMut();
    state.xform = nvgTransformPremultiply(state.xform, t);
}

pub fn nvgResetTransform(ctx: &mut NVGcontext) {
    ctx.getStateMut().xform = nvgTransformIdentity();
}

pub fn nvgTranslate(ctx: &mut NVGcontext, x: f32, y: f32) {
    let state = ctx.getStateMut();
    state.xform = nvgTransformPremultiply(state.xform, nvgTransformTranslate(x, y));
}

pub fn nvgRotate(ctx: &mut NVGcontext, angle: f32) {
    let state = ctx.getStateMut();
    state.xform = nvgTransformPremultiply(state.xform, nvgTransformRotate(angle));
}

pub fn nvgSkewX(ctx: &mut NVGcontext, angle: f32) {
    let state = ctx.getStateMut();
    state.xform = nvgTransformPremultiply(state.xform, nvgTransformSkewX(angle));
}

pub fn nvgSkewY(ctx: &mut NVGcontext, angle: f32) {
    let state = ctx.getStateMut();
    state.xform = nvgTransformPremultiply(state.xform, nvgTransformSkewY(angle));
}

pub fn nvgScale(ctx: &mut NVGcontext, x: f32, y: f32) {
    let state = ctx.getStateMut();
    state.xform = nvgTransformPremultiply(state.xform, nvgTransformScale(x, y));
}

pub fn nvgCurrentTransform(ctx: &NVGcontext) -> NVGtransform {
    ctx.getState().xform
}

pub fn nvgStrokeColor(ctx: &mut NVGcontext, color: NVGcolor) {
    ctx.getStateMut().stroke = NVGpaint::color(color);
}

pub fn nvgStrokePaint(ctx: &mut NVGcontext, paint: NVGpaint) {
    let state = ctx.getStateMut();
    state.stroke = paint;
    state.stroke.xform = nvgTransformMultiply(state.stroke.xform, state.xform);
}

pub fn nvgFillColor(ctx: &mut NVGcontext, color: NVGcolor) {
    ctx.getStateMut().fill = NVGpaint::color(color);
}

pub fn nvgFillPaint(ctx: &mut NVGcontext, paint: NVGpaint) {
    let state = ctx.getStateMut();
    state.fill = paint;
    state.fill.xform = nvgTransformMultiply(state.fill.xform, state.xform);
}

pub fn nvgLinearGradient(
    sx: f32,
    sy: f32,
    ex: f32,
    ey: f32,
    icol: NVGcolor,
    ocol: NVGcolor,
) -> NVGpaint {
    let large = 1.0e5;
    let mut dx = ex - sx;
    let mut dy = ey - sy;
    let d = nvg__hypotenuse(dx, dy);
    if d > 1.0e-4 {
        dx /= d;
        dy /= d;
    } else {
        dx = 0.0;
        dy = 1.0;
    }

    NVGpaint {
        xform: [dy, -dx, dx, dy, sx - dx * large, sy - dy * large],
        extent: [large, large + d * 0.5],
        radius: 0.0,
        feather: nvg__max(1.0, d),
        innerColor: icol,
        outerColor: ocol,
    }
}

pub fn nvgRadialGradient(
    cx: f32,
    cy: f32,
    inr: f32,
    outr: f32,
    icol: NVGcolor,
    ocol: NVGcolor,
) -> NVGpaint {
    let r = (inr + outr) * 0.5;
    let f = outr - inr;
    NVGpaint {
        xform: [1.0, 0.0, 0.0, 1.0, cx, cy],
        extent: [r, r],
        radius: r,
        feather: nvg__max(1.0, f),
        innerColor: icol,
        outerColor: ocol,
    }
}

pub fn nvgBoxGradient(
    x: f32,
    y: f32,
    w: f32,
    h: f32,
    r: f32,
    f: f32,
    icol: NVGcolor,
    ocol: NVGcolor,
) -> NVGpaint {
    NVGpaint {
        xform: [1.0, 0.0, 0.0, 1.0, x + w * 0.5, y + h * 0.5],
        extent: [w * 0.5, h * 0.5],
        radius: r,
        feather: nvg__max(1.0, f),
        innerColor: icol,
        outerColor: ocol,
    }
}

pub fn nvgScissor(ctx: &mut NVGcontext, x: f32, y: f32, w: f32, h: f32) {
    let state = ctx.getStateMut();
    let w = nvg__max(0.0, w);
    let h = nvg__max(0.0, h);
    let xform = [1.0, 0.0, 0.0, 1.0, x + w * 0.5, y + h * 0.5];
    state.scissor.xform = nvgTransformMultiply(xform, state.xform);
    state.scissor.extent = [w * 0.5, h * 0.5];
}

fn nvg__isectRects(ax: f32, ay: f32, aw: f32, ah: f32,
                   bx: f32, by: f32, bw: f32, bh: f32) -> [f32; 4] {
    let minx = nvg__max(ax, bx);
    let miny = nvg__max(ay, by);
    let maxx = nvg__min(ax + aw, bx + bw);
    let maxy = nvg__min(ay + ah, by + bh);
    [minx, miny, nvg__max(0.0, maxx - minx), nvg__max(0.0, maxy - miny)]
}

pub fn nvgIntersectScissor(ctx: &mut NVGcontext, x: f32, y: f32, w: f32, h: f32) {
    if ctx.getState().scissor.extent[0] < 0.0 {
        nvgScissor(ctx, x, y, w, h);
        return;
    }

    let rect = {
        let state = ctx.getStateMut();
        let pxform = state.scissor.xform;
        let ex = state.scissor.extent[0];
        let ey = state.scissor.extent[1];
        let invxorm = nvgTransformInverse(state.xform).unwrap_or_else(|| nvgTransformIdentity());
        let pxform = nvgTransformMultiply(pxform, invxorm);
        let tex = ex * pxform[0].abs() + ey * pxform[2].abs();
        let tey = ex * pxform[1].abs() + ey * pxform[3].abs();
        nvg__isectRects(pxform[4] - tex, pxform[5] - tey, tex * 2.0, tey * 2.0, x, y, w, h)
    };

    nvgScissor(ctx, rect[0], rect[1], rect[2], rect[3]);
}

pub fn nvgResetScissor(ctx: &mut NVGcontext) {
    ctx.getStateMut().scissor = NVGscissor::new();
}

pub fn nvgGlobalCompositeOperation(ctx: &mut NVGcontext, op: NVGcompositeOperation) {
    ctx.getStateMut().compositeOperation = nvg__compositeOperationState(op);
}

pub fn nvgGlobalCompositeBlendFunc(
    ctx: &mut NVGcontext,
    sfactor: NVGblendFactor,
    dfactor: NVGblendFactor,
) {
    nvgGlobalCompositeBlendFuncSeparate(ctx, sfactor, dfactor, sfactor, dfactor);
}

pub fn nvgGlobalCompositeBlendFuncSeparate(
    ctx: &mut NVGcontext,
    srcRGB: NVGblendFactor,
    dstRGB: NVGblendFactor,
    srcAlpha: NVGblendFactor,
    dstAlpha: NVGblendFactor,
) {
    ctx.getStateMut().compositeOperation = NVGcompositeOperationState {
        srcRGB,
        dstRGB,
        srcAlpha,
        dstAlpha,
    };
}

fn nvg__ptEquals(x1: f32, y1: f32, x2: f32, y2: f32, tol: f32) -> bool {
    let dx = x2 - x1;
    let dy = y2 - y1;
    return dx * dx + dy * dy < tol * tol;
}

fn nvg__distPtSeg(x: f32, y: f32, px: f32, py: f32, qx: f32, qy: f32) -> f32 {
    let pqx = qx - px;
    let pqy = qy - py;
    let dx = x - px;
    let dy = y - py;
    let d = pqx * pqx + pqy * pqy;
    let mut t = pqx * dx + pqy * dy;

    if d > 0.0 {
        t /= d;
    }

    if t < 0.0 {
        t = 0.0;
    } else if t > 1.0 {
        t = 1.0;
    }

    let dx = px + t * pqx - x;
    let dy = py + t * pqy - y;

    return dx * dx + dy * dy;
}

fn nvg__appendCommands(ctx: &mut NVGcontext, commands: Vec<NVGcommand>) {
    let xform = ctx.getState().xform;

    reserve!(ctx.commands, commands.len());

    for command in commands.into_iter() {
        ctx.commands.push(match command {
            NVGcommand::MoveTo { x, y } => {
                ctx.commandx = x;
                ctx.commandy = y;
                let (x, y) = nvgTransformPoint(&xform, x, y);
                NVGcommand::MoveTo { x, y }
            },
            NVGcommand::LineTo { x, y } => {
                ctx.commandx = x;
                ctx.commandy = y;
                let (x, y) = nvgTransformPoint(&xform, x, y);
                NVGcommand::LineTo { x, y }
            },
            NVGcommand::BezierTo { c1x, c1y, c2x, c2y, x, y } => {
                ctx.commandx = x;
                ctx.commandy = y;
                let (c1x, c1y) = nvgTransformPoint(&xform, c1x, c1y);
                let (c2x, c2y) = nvgTransformPoint(&xform, c2x, c2y);
                let (x, y) = nvgTransformPoint(&xform, x, y);
                NVGcommand::BezierTo { c1x, c1y, c2x, c2y, x, y }
            },
            c => c,
        });
    }
}

fn nvg__addPath(cache: &mut NVGpathCache) {
    let npoints = cache.points.len();
    cache.paths.push(NVGpath {
        first: npoints as i32,
        count: 0,
        closed: 0,
        nbevel: 0,
        fillFirst: 0,
        fillCount: 0,
        strokeFirst: 0,
        strokeCount: 0,
        convex: 0,
        winding: NVG_CCW,
    });
}

fn nvg__addPoint(cache: &mut NVGpathCache, distTol: f32, x: f32, y: f32, flags: NVGpointFlag) {
    let path = match cache.paths.last_mut() {
        Some(p) => p,
        None => return,
    };

    if path.count > 0 && cache.points.len() > 0 {
        match cache.points.last_mut() {
            Some(pt) => {
                if nvg__ptEquals(pt.x, pt.y, x, y, distTol) {
                    pt.flags |= flags;
                    return;
                }
            },
            None => unreachable!(),
        }
    }

    path.count += 1;

    cache.points.push(NVGpoint {
        x,
        y,
        dx: 0.0,
        dy: 0.0,
        len: 0.0,
        dmx: 0.0,
        dmy: 0.0,
        flags: flags,
    });
}

fn nvg__closePath(cache: &mut NVGpathCache) {
    if let Some(path) = cache.paths.last_mut() {
        path.closed = 1;
    }
}

fn nvg__pathWinding(cache: &mut NVGpathCache, winding: NVGwinding) {
    if let Some(path) = cache.paths.last_mut() {
        path.winding = winding;
    }
}

fn nvg__getAverageScale(t: &NVGtransform) -> f32 {
    let sx = (t[0] * t[0] + t[2] * t[2]).sqrt();
    let sy = (t[1] * t[1] + t[3] * t[3]).sqrt();
    (sx + sy) * 0.5
}

fn nvg__triarea2(ax: f32, ay: f32, bx: f32, by: f32, cx: f32, cy: f32) -> f32 {
    (cx - ax) * (by - ay) - (bx - ax) * (cy - ay)
}

fn nvg__polyArea(points: &[NVGpoint]) -> f32 {
    let mut area = 0.0;

    for window in points.windows(3) {
        let a = &window[0];
        let b = &window[1];
        let c = &window[2];
        area += nvg__triarea2(a.x, a.y, b.x, b.y, c.x, c.y);
    }

    area * 0.5
}

fn nvg__vmake(verts: &mut Vec<NVGvertex>, x: f32, y: f32, u: f32, v: f32) {
    verts.push(NVGvertex { x, y, u, v });
}

fn nvg__tesselateBezier(cache: &mut NVGpathCache, tessTol: f32, distTol: f32,
                        x1: f32, y1: f32, x2: f32, y2: f32,
                        x3: f32, y3: f32, x4: f32, y4: f32,
                        level: i32, flags: NVGpointFlag) {
    if level > 10 {
        return;
    }

    let x12 = (x1 + x2) * 0.5;
	let y12 = (y1 + y2) * 0.5;
	let x23 = (x2 + x3) * 0.5;
	let y23 = (y2 + y3) * 0.5;
	let x34 = (x3 + x4) * 0.5;
	let y34 = (y3 + y4) * 0.5;
	let x123 = (x12 + x23) * 0.5;
	let y123 = (y12 + y23) * 0.5;

    let dx = x4 - x1;
    let dy = y4 - y1;
    let d2 = ((x2 - x4) * dy - (y2 - y4) * dx).abs();
    let d3 = ((x3 - x4) * dy - (y3 - y4) * dx).abs();

    if (d2 + d3) * (d2 + d3) < tessTol * (dx * dx + dy * dy) {
        nvg__addPoint(cache, distTol, x4, y4, flags);
        return;
    }

    let x234 = (x23 + x34) * 0.5;
    let y234 = (y23 + y34) * 0.5;
	let x1234 = (x123 + x234) * 0.5;
	let y1234 = (y123 + y234) * 0.5;

	nvg__tesselateBezier(cache, tessTol, distTol, x1, y1, x12, y12, x123,
        y123, x1234, y1234, level + 1, 0);
	nvg__tesselateBezier(cache, tessTol, distTol, x1234, y1234, x234, y234,
        x34, y34, x4, y4, level + 1, flags);
}

fn nvg__flattenPaths(ctx: &mut NVGcontext) {
    if ctx.cache.paths.len() > 0 {
        return;
    }

    for command in ctx.commands.iter() {
        match command {
            NVGcommand::MoveTo { x, y } => {
                nvg__addPath(&mut ctx.cache);
                nvg__addPoint(&mut ctx.cache, ctx.distTol, *x, *y, NVG_PT_CORNER);
            },
            NVGcommand::LineTo { x, y } => {
                nvg__addPoint(&mut ctx.cache, ctx.distTol, *x, *y, NVG_PT_CORNER);
            },
            NVGcommand::BezierTo { c1x, c1y, c2x, c2y, x, y } => {
                if ctx.cache.points.len() > 0 {
                    let lastX = ctx.cache.points.last().unwrap().x;
                    let lastY = ctx.cache.points.last().unwrap().y;
                    nvg__tesselateBezier(&mut ctx.cache, ctx.tessTol, ctx.distTol,
                        lastX, lastY, *c1x, *c1y, *c2x, *c2y, *x, *y, 0, NVG_PT_CORNER);
                }
            },
            NVGcommand::Close => {
                nvg__closePath(&mut ctx.cache);
            },
            NVGcommand::Winding(winding) => {
                nvg__pathWinding(&mut ctx.cache, *winding);
            },
        }
    }

    ctx.cache.bounds[0] = 1.0e6;
    ctx.cache.bounds[1] = 1.0e6;
    ctx.cache.bounds[2] = -1.0e6;
    ctx.cache.bounds[3] = -1.0e6;

    for path in ctx.cache.paths.iter_mut() {
        let pstart = path.first as usize;
        let pend = path.first as usize + path.count as usize;
        let p = &mut ctx.cache.points[pstart..pend];
        let mut i0 = (path.count - 1) as usize;
        let mut i1 = 0 as usize;
        if nvg__ptEquals(p[i0].x, p[i0].y, p[i1].x, p[i1].y, ctx.distTol) {
            path.count -= 1;
            i0 = (path.count - 1) as usize;
            path.closed = 1;
        }

        if path.count > 2 {
            let area = nvg__polyArea(p);
            if (path.winding == NVG_CCW && area < 0.0) || (path.winding == NVG_CW && area > 0.0) {
                p.reverse();
            }
        }

        for _ in 0..path.count {
            p[i0].dx = p[i1].x - p[i0].x;
            p[i0].dy = p[i1].y - p[i0].y;
            p[i0].len = nvg__hypotenuse(p[i0].dx, p[i0].dy);
            let norm = nvg__normalize(p[i0].dx, p[i0].dy);
            p[i0].dx = norm.0;
            p[i0].dy = norm.1;
            ctx.cache.bounds[0] = nvg__min(ctx.cache.bounds[0], p[i0].x);
            ctx.cache.bounds[1] = nvg__min(ctx.cache.bounds[1], p[i0].y);
            ctx.cache.bounds[2] = nvg__min(ctx.cache.bounds[2], p[i0].x);
            ctx.cache.bounds[3] = nvg__min(ctx.cache.bounds[3], p[i0].y);
            i0 = i1;
            i1 += 1;
        }
    }
}

fn nvg__curveDivs(r: f32, arc: f32, tol: f32) -> i32 {
    let da = (r / (r + tol) * 2.0).acos();
    nvg__max(2, (arc / da).ceil() as i32)
}

fn nvg__chooseBevel(bevel: bool, p0: &NVGpoint, p1: &NVGpoint, w: f32) -> ((f32, f32), (f32, f32)) {
    if bevel {
        ((p1.x + p0.dy * w, p1.y - p0.dx * w), (p1.x + p1.dy * w, p1.y - p1.dx * w))
    } else {
        ((p1.x + p1.dmx * w, p1.y + p1.dmy * w), (p1.x + p1.dmx * w, p1.y + p1.dmy * w))
    }
}

fn nvg__roundJoin(verts: &mut Vec<NVGvertex>, p0: &NVGpoint, p1: &NVGpoint,
                  lw: f32, rw: f32, lu: f32, ru: f32, ncap: i32) {
    let dlx0 = p0.dy;
    let dly0 = -p0.dx;
    let dlx1 = p1.dy;
    let dly1 = -p1.dx;

    if p1.flags & NVG_PT_LEFT != 0 {
        let bevel = p1.flags & NVG_PR_INNERBEVEL != 0;
		let ((lx0, ly0), (lx1, ly1)) = nvg__chooseBevel(bevel, p0, p1, lw);
        let a0 = (-dly0).atan2(-dlx0);
        let mut a1 = (-dly1).atan2(-dlx1);
        if a1 > a0 {
            a1 -= (NVG_PI * 2.0) as f32;
        }
        nvg__vmake(verts, lx0, ly0, lu, 1.0);
        nvg__vmake(verts, p1.x - dlx0 * rw, p1.y - dly0 * rw, ru, 1.0);

        let n = nvg__clamp((((a0 - a1) / NVG_PI as f32) * ncap as f32).ceil() as i32, 2, ncap);
        for j in 0..n {
            let u = j as f32 / (n - 1) as f32;
            let a = a0 + u * (a1 - a0);
            let rx = p1.x + a.cos() * rw;
            let ry = p1.y + a.sin() * rw;
            nvg__vmake(verts, p1.x, p1.y, 0.5, 1.0);
            nvg__vmake(verts, rx, ry, ru, 1.0);
        }
        nvg__vmake(verts, lx1, ly1, lu, 1.0);
        nvg__vmake(verts, p1.x - dlx1 * rw, p1.y - dly1 * rw, ru, 1.0);
    } else {
        let bevel = p1.flags & NVG_PR_INNERBEVEL != 0;
		let ((rx0, ry0), (rx1, ry1)) = nvg__chooseBevel(bevel, p0, p1, -rw);
        let a0 = dly0.atan2(dlx0);
        let mut a1 = dly1.atan2(dlx1);
        if a1 < a0 {
            a1 += (NVG_PI * 2.0) as f32;
        }
        nvg__vmake(verts, p1.x + dlx0 * rw, p1.y + dly0 * rw, lu, 1.0);
        nvg__vmake(verts, rx0, ry0, ru, 1.0);
        let n = nvg__clamp((((a1 - a0) / NVG_PI as f32) * ncap as f32).ceil() as i32, 2, ncap);
        for j in 0..n {
            let u = j as f32 / (n - 1) as f32;
            let a = a0 + u * (a1 - a0);
            let lx = p1.x + a.cos() * lw;
            let ly = p1.y + a.sin() * lw;
            nvg__vmake(verts, p1.x, p1.y, 0.5, 1.0);
            nvg__vmake(verts, lx, ly, lu, 1.0);
        }
        nvg__vmake(verts, p1.x - dlx1 * rw, p1.y - dly1 * rw, lu, 1.0);
        nvg__vmake(verts, rx1, ry1, ru, 1.0);
    }
}

fn nvg__bevelJoin(verts: &mut Vec<NVGvertex>, p0: &NVGpoint, p1: &NVGpoint,
                  lw: f32, rw: f32, lu: f32, ru: f32)
{
    let dlx0 = p0.dy;
    let dly0 = -p0.dx;
    let dlx1 = p1.dy;
    let dly1 = -p1.dx;

    if p1.flags & NVG_PT_LEFT != 0 {
        let bevel = p1.flags & NVG_PR_INNERBEVEL != 0;
		let ((lx0, ly0), (lx1, ly1)) = nvg__chooseBevel(bevel, p0, p1, lw);
        nvg__vmake(verts, lx0, ly0, lu, 1.0);
        nvg__vmake(verts, p1.x - dlx0 * rw, p1.y - dly0 * rw, ru, 1.0);

        if p1.flags & NVG_PT_BEVEL != 0 {
            nvg__vmake(verts, lx0, ly0, lu, 1.0);
            nvg__vmake(verts, p1.x - dlx0 * rw, p1.y - dly0 * rw, ru, 1.0);

            nvg__vmake(verts, lx1, ly1, lu, 1.0);
            nvg__vmake(verts, p1.x - dlx1 * rw, p1.y - dly1 * rw, ru, 1.0);
        } else {
            let rx0 = p1.x - p1.dmx * rw;
            let ry0 = p1.y - p1.dmy * rw;

            nvg__vmake(verts, p1.x, p1.y, 0.5, 1.0);
            nvg__vmake(verts, p1.x - dlx0 * rw, p1.y - dly0 * rw, ru, 1.0);

            nvg__vmake(verts, rx0, ry0, ru, 1.0);
            nvg__vmake(verts, rx0, ry0, ru, 1.0);

            nvg__vmake(verts, p1.x, p1.y, 0.5, 1.0);
            nvg__vmake(verts, p1.x - dlx1 * rw, p1.y - dly1 * rw, ru, 1.0);
        }

        nvg__vmake(verts, lx1, ly1, lu, 1.0);
        nvg__vmake(verts, p1.x - dlx1 * rw, p1.y - dly1 * rw, ru, 1.0);
    } else {
        let bevel = p1.flags & NVG_PR_INNERBEVEL != 0;
		let ((rx0, ry0), (rx1, ry1)) = nvg__chooseBevel(bevel, p0, p1, -rw);
        nvg__vmake(verts, p1.x - dlx0 * lw, p1.y - dly0 * lw, lu, 1.0);
        nvg__vmake(verts, rx0, ry0, ru, 1.0);

        if p1.flags & NVG_PT_BEVEL != 0 {
            nvg__vmake(verts, p1.x + dlx0 * lw, p1.y + dly0 * lw, lu, 1.0);
            nvg__vmake(verts, rx0, ry0, ru, 1.0);

            nvg__vmake(verts, p1.x + dlx1 * lw, p1.y + dly1 * lw, lu, 1.0);
            nvg__vmake(verts, rx1, ry1, ru, 1.0);
        } else {
            let lx0 = p1.x + p1.dmx * lw;
            let ly0 = p1.y + p1.dmy * lw;

            nvg__vmake(verts, p1.x + dlx0 * lw, p1.y + dly0 * lw, lu, 1.0);
            nvg__vmake(verts, p1.x, p1.y, 0.5, 1.0);

            nvg__vmake(verts, lx0, ly0, lu, 1.0);
            nvg__vmake(verts, lx0, ly0, lu, 1.0);

            nvg__vmake(verts, p1.x + dlx1 * lw, p1.y + dly1 * lw, lu, 1.0);
            nvg__vmake(verts, p1.x, p1.y, 0.5, 1.0);
        }

        nvg__vmake(verts, p1.x + dlx1 * lw, p1.y + dly1 * lw, lu, 1.0);
        nvg__vmake(verts, rx1, ry1, ru, 1.0);
    }
}

fn nvg__buttCapStart(verts: &mut Vec<NVGvertex>, p: &NVGpoint,
                     dx: f32, dy: f32, w: f32, d: f32, aa: f32, u0: f32, u1: f32)
{
    let px = p.x - dx * d;
    let py = p.y - dy * d;
    let dlx = dy;
    let dly = -dx;

    nvg__vmake(verts, px + dlx * w - dx * aa, py + dly * w - dy * aa, u0, 0.0);
    nvg__vmake(verts, px - dlx * w - dx * aa, py - dly * w - dy * aa, u1, 0.0);
    nvg__vmake(verts, px + dlx * w, py + dly * w, u0, 1.0);
    nvg__vmake(verts, px - dlx * w, py - dly * w, u1, 1.0);
}

fn nvg__buttCapEnd(verts: &mut Vec<NVGvertex>, p: &NVGpoint,
                   dx: f32, dy: f32, w: f32, d: f32, aa: f32, u0: f32, u1: f32)
{
    let px = p.x + dx * d;
    let py = p.y + dy * d;
    let dlx = dy;
    let dly = -dx;

    nvg__vmake(verts, px + dlx * w, py + dly * w, u0, 1.0);
    nvg__vmake(verts, px - dlx * w, py - dly * w, u1, 1.0);
    nvg__vmake(verts, px + dlx * w + dx * aa, py + dly * w + dy * aa, u0, 0.0);
    nvg__vmake(verts, px - dlx * w + dx * aa, py - dly * w + dy * aa, u1, 0.0);
}

fn nvg__roundCapStart(verts: &mut Vec<NVGvertex>, p: &NVGpoint,
                      dx: f32, dy: f32, w: f32, ncap: i32, u0: f32, u1: f32)
{
    let px = p.x;
    let py = p.y;
    let dlx = dy;
    let dly = -dx;

    for j in 0..ncap {
        let a = j as f32 / (ncap - 1) as f32 * NVG_PI as f32;
        let ax = a.cos() * w;
        let ay = a.sin() * w;
        nvg__vmake(verts, px - dlx * ax - dx * ay, py - dly * ax - dy * ay, u0, 1.0);
        nvg__vmake(verts, px, py, 0.5, 1.0);
    }

    nvg__vmake(verts, px + dlx * w, py + dly * w, u0, 1.0);
    nvg__vmake(verts, px - dlx * w, py - dly * w, u1, 1.0);
}

fn nvg__roundCapEnd(verts: &mut Vec<NVGvertex>, p: &NVGpoint,
                    dx: f32, dy: f32, w: f32, ncap: i32, u0: f32, u1: f32)
{
    let px = p.x;
    let py = p.y;
    let dlx = dy;
    let dly = -dx;

    nvg__vmake(verts, px + dlx * w, py + dly * w, u0, 1.0);
    nvg__vmake(verts, px - dlx * w, py - dly * w, u1, 1.0);

    for j in 0..ncap {
        let a = j as f32 / (ncap - 1) as f32 * NVG_PI as f32;
        let ax = a.cos() * w;
        let ay = a.sin() * w;
        nvg__vmake(verts, px, py, 0.5, 1.0);
        nvg__vmake(verts, px - dlx * ax + dx * ay, py - dly * ax + dy * ay, u0, 1.0);
    }
}

fn nvg__calculateJoins(cache: &mut NVGpathCache, w: f32, lineJoin: NVGlineCap, miterLimit: f32) {
    let iw = if w > 0.0 { 1.0 / w } else { 0.0 };
    for path in cache.paths.iter_mut() {
        let pstart = path.first as usize;
        let pend = path.first as usize + path.count as usize;
        let p = &mut cache.points[pstart..pend];
        let mut i0 = (path.count - 1) as usize;
        let mut i1 = 0 as usize;
        let mut nleft = 0;
        path.nbevel = 0;

        for _ in 0..path.count {
            let dlx0 = p[i0].dy;
            let dly0 = -p[i0].dx;
            let dlx1 = p[i1].dy;
            let dly1 = -p[i1].dx;

            p[i1].dmx = (dlx0 + dlx1) * 0.5;
            p[i1].dmy = (dly0 + dly1) * 0.5;
            let dmr2 = p[i1].dmx * p[i1].dmx + p[i1].dmy * p[i1].dmy;
            if dmr2 > 1.0e-6 {
                let mut scale = 1.0 / dmr2;
                if scale > 600.0 {
                    scale = 600.0;
                }
                p[i1].dmx *= scale;
                p[i1].dmy *= scale;
            }

			p[i1].flags &= NVG_PT_CORNER;

            let nvg__cross = p[i1].dx * p[i0].dy - p[i0].dx * p[i1].dy;
            if nvg__cross > 0.0 {
                nleft += 1;
                p[i1].flags |= NVG_PT_LEFT;
            }

            let limit = nvg__max(1.01, nvg__min(p[i0].len, p[i1].len) * iw);
            if (dmr2 * limit * limit) < 1.0 {
                p[i1].flags |= NVG_PR_INNERBEVEL;
            }

            if p[i1].flags & NVG_PT_CORNER != 0 {
                let lim = dmr2 * miterLimit * miterLimit;
                if lim < 1.0 || lineJoin == NVG_BEVEL || lineJoin == NVG_ROUND {
                    p[i1].flags |= NVG_PT_BEVEL;
                }
            }

            if p[i1].flags & (NVG_PT_BEVEL | NVG_PR_INNERBEVEL) != 0 {
                path.nbevel += 1;
            }

            i0 = i1;
            i1 += 1;
        }

        path.convex = if nleft == path.count { 1 } else { 0 };
    }
}

fn nvg__expandStroke(ctx: &mut NVGcontext, w: f32, fringe: f32, lineCap: NVGlineCap,
                     lineJoin: NVGlineCap, miterLimit: f32) {
    let aa = fringe;
    let mut u0 = 0.0;
    let mut u1 = 1.0;
    let ncap = nvg__curveDivs(w, NVG_PI as f32, ctx.tessTol);
    let w = w + aa * 0.5;

    if aa == 0.0 {
        u0 = 0.5;
        u1 = 0.5;
    }

    nvg__calculateJoins(&mut ctx.cache, w, lineJoin, miterLimit);

    let mut cverts = 0;
    for path in ctx.cache.paths.iter() {
        let isLoop = path.closed != 0;
        if lineJoin == NVG_ROUND {
            cverts += (path.count + path.nbevel * (ncap + 2) + 1) * 2;
        } else {
            cverts += (path.count + path.nbevel * 5 + 1) * 2;
        }
        if !isLoop {
            if lineCap == NVG_ROUND {
                cverts += (ncap * 2 + 2) * 2;
            } else {
                cverts += (3 + 3) * 2;
            }
        }
    }

    reserve!(ctx.cache.verts, cverts);

    for path in ctx.cache.paths.iter_mut() {
        let pstart = path.first as usize;
        let pend = path.first as usize + path.count as usize;
        let p = &mut ctx.cache.points[pstart..pend];
        let mut i0;
        let mut i1;
        let s;
        let e;

        let isLoop = path.closed != 0;
        path.strokeFirst = ctx.cache.verts.len();

        if isLoop {
            i0 = (path.count - 1) as usize;
            i1 = 0 as usize;
            s = 0;
            e = path.count;
        } else {
            i0 = 0 as usize;
            i1 = 1 as usize;
            s = 1;
            e = path.count - 1;
        }

        if !isLoop {
            let dx = p[i1].x - p[i0].x;
            let dy = p[i1].y - p[i0].y;
            let (dx, dy) = nvg__normalize(dx, dy);
            if lineCap == NVG_BUTT {
                nvg__buttCapStart(&mut ctx.cache.verts, &p[i0], dx, dy, w, -aa * 0.5, aa, u0, u1);
            } else if lineCap == NVG_SQUARE {
                nvg__buttCapStart(&mut ctx.cache.verts, &p[i0], dx, dy, w, w - aa, aa, u0, u1);
            } else if lineCap == NVG_ROUND {
                nvg__roundCapStart(&mut ctx.cache.verts, &p[i0], dx, dy, w, ncap, u0, u1);
            }
        }

        for _ in s..e {
            if p[i1].flags & (NVG_PT_BEVEL | NVG_PR_INNERBEVEL) != 0 {
                if lineJoin == NVG_ROUND {
                    nvg__roundJoin(&mut ctx.cache.verts, &p[i0], &p[i1], w, w, u0, u1, ncap);
                } else {
                    nvg__bevelJoin(&mut ctx.cache.verts, &p[i0], &p[i1], w, w, u0, u1);
                }
            } else {
                nvg__vmake(&mut ctx.cache.verts, p[i1].x + (p[i1].dmx * w),
                           p[i1].y + (p[i1].dmy * w), u0, 1.0);
                nvg__vmake(&mut ctx.cache.verts, p[i1].x - (p[i1].dmx * w),
                           p[i1].y - (p[i1].dmy * w), u1, 1.0);
            }
            i0 = i1;
            i1 += 1;
        }

        if isLoop {
            let v0 = ctx.cache.verts[path.strokeFirst];
            nvg__vmake(&mut ctx.cache.verts, v0.x, v0.y, u0, 1.0);
            let v1 = ctx.cache.verts[path.strokeFirst + 1];
            nvg__vmake(&mut ctx.cache.verts, v1.x, v1.y, u0, 1.0);
        } else {
            let dx = p[i1].x - p[i0].x;
            let dy = p[i1].y - p[i0].y;
            let (dx, dy) = nvg__normalize(dx, dy);
            if lineCap == NVG_BUTT {
                nvg__buttCapEnd(&mut ctx.cache.verts, &p[i1], dx, dy, w, -aa * 0.5, aa, u0, u1);
            } else if lineCap == NVG_SQUARE {
                nvg__buttCapEnd(&mut ctx.cache.verts, &p[i1], dx, dy, w, w - aa, aa, u0, u1);
            } else if lineCap == NVG_ROUND {
                nvg__roundCapEnd(&mut ctx.cache.verts, &p[i1], dx, dy, w, ncap, u0, u1);
            }
        }

        path.strokeCount = (ctx.cache.verts.len() - path.strokeFirst) as i32;
    }
}

fn nvg__expandFill(ctx: &mut NVGcontext, w: f32, lineJoin: NVGlineCap, miterLimit: f32) {
    let aa = ctx.fringeWidth;
    let fringe = w > 0.0;

    nvg__calculateJoins(&mut ctx.cache, w, lineJoin, miterLimit);

    let mut cverts = 0;
    for path in ctx.cache.paths.iter() {
        cverts += path.count + path.nbevel + 1;
        if fringe {
            cverts += (path.count + path.nbevel * 5 + 1) * 2;
        }
    }
    reserve!(ctx.cache.verts, cverts);

    let convex = ctx.cache.paths.len() == 1 && ctx.cache.paths[0].convex != 0;

    for path in ctx.cache.paths.iter_mut() {
        let pstart = path.first as usize;
        let pend = path.first as usize + path.count as usize;
        let p = &mut ctx.cache.points[pstart..pend];

        let woff = 0.5 * aa;
        path.fillFirst = ctx.cache.verts.len();

        if fringe {
            let mut i0 = (path.count - 1) as usize;
            let mut i1 = 0 as usize;
            for _ in 0..path.count as usize {
                if p[i0].flags & NVG_PT_BEVEL != 0 {
                    let dlx0 = p[i0].dy;
                    let dly0 = -p[i0].dx;
                    let dlx1 = p[i1].dy;
                    let dly1 = -p[i1].dx;
                    if p[i1].flags & NVG_PT_LEFT != 0 {
                        let lx = p[i1].x + p[i1].dmx * woff;
                        let ly = p[i1].y + p[i1].dmy * woff;
                        nvg__vmake(&mut ctx.cache.verts, lx, ly, 0.5, 1.0);
                    } else {
                        let lx0 = p[i1].x + dlx0 * woff;
                        let ly0 = p[i1].y + dly0 * woff;
                        let lx1 = p[i1].x + dlx1 * woff;
                        let ly1 = p[i1].y + dly1 * woff;
                        nvg__vmake(&mut ctx.cache.verts, lx0, ly0, 0.5, 1.0);
                        nvg__vmake(&mut ctx.cache.verts, lx1, ly1, 0.5, 1.0);
                    }
                } else {
                    nvg__vmake(&mut ctx.cache.verts, p[i1].x + (p[i1].dmx * woff),
                        p[i1].y + (p[i1].dmy * woff), 0.5, 1.0);
                }
                i0 = i1;
                i1 += 1;
            }
        } else {
            for j in 0..path.count as usize {
                nvg__vmake(&mut ctx.cache.verts, p[j].x, p[j].y, 0.5, 1.0);
            }
        }

        path.fillCount = (ctx.cache.verts.len() - path.fillFirst) as i32;

        if fringe {
            let mut lw = w + woff;
            let rw = w - woff;
            let mut lu = 0.0;
            let ru = 1.0;
            path.strokeFirst = ctx.cache.verts.len();

            if convex {
                lw = woff;
                lu = 0.5;
            }

            let mut i0 = (path.count - 1) as usize;
            let mut i1 = 0 as usize;

            for _ in 0..path.count as usize {
                if p[i0].flags & (NVG_PT_BEVEL | NVG_PR_INNERBEVEL) != 0 {
                    nvg__bevelJoin(&mut ctx.cache.verts, &p[i0], &p[i1], lw, rw, lu, ru);
                } else {
                    nvg__vmake(&mut ctx.cache.verts, p[i1].x + (p[i1].dmx * lw),
                        p[i1].y + (p[i1].dmy * lw), lu, 1.0);
                    nvg__vmake(&mut ctx.cache.verts, p[i1].x - (p[i1].dmx * rw),
                        p[i1].y - (p[i1].dmy * rw), ru, 1.0);
                }
                i0 = i1;
                i1 += 1;
            }

            let v0 = ctx.cache.verts[path.strokeFirst];
            nvg__vmake(&mut ctx.cache.verts, v0.x, v0.y, lu, 1.0);
            let v1 = ctx.cache.verts[path.strokeFirst + 1];
            nvg__vmake(&mut ctx.cache.verts, v1.x, v1.y, ru, 1.0);

            path.strokeCount = (ctx.cache.verts.len() - path.strokeFirst) as i32;
        } else {
            path.strokeFirst = 0;
            path.strokeCount = 0;
        }
    }
}

pub fn nvgBeginPath(ctx: &mut NVGcontext) {
    ctx.commands.clear();
    ctx.cache.points.clear();
    ctx.cache.paths.clear();

    // So, this isn't how NanoVG manages its verts. All of its verts are "temporary" for just the
    // render call. See `nvg__allocTempVerts`.
    ctx.cache.verts.clear();
}

pub fn nvgMoveTo(ctx: &mut NVGcontext, x: f32, y: f32) {
    nvg__appendCommands(ctx, vec![NVGcommand::MoveTo { x, y }]);
}

pub fn nvgLineTo(ctx: &mut NVGcontext, x: f32, y: f32) {
    nvg__appendCommands(ctx, vec![NVGcommand::LineTo { x, y }]);
}

pub fn nvgBezierTo(ctx: &mut NVGcontext, c1x: f32, c1y: f32, c2x: f32, c2y: f32, x: f32, y: f32) {
    nvg__appendCommands(ctx, vec![NVGcommand::BezierTo { c1x, c1y, c2x, c2y, x, y }]);
}

pub fn nvgQuadTo(ctx: &mut NVGcontext, cx: f32, cy: f32, x: f32, y: f32) {
    let x0 = ctx.commandx;
    let y0 = ctx.commandy;
    nvg__appendCommands(ctx, vec![NVGcommand::BezierTo {
        c1x: x0 + 2.0 / 3.0 * (cx - x0),
        c1y: y0 + 2.0 / 3.0 * (cy - y0),
        c2x: x + 2.0 / 3.0 * (cx - x),
        c2y: y + 2.0 / 3.0 * (cy - y),
        x,
        y,
    }]);
}

pub fn nvgArcTo(ctx: &mut NVGcontext, x1: f32, y1: f32, x2: f32, y2: f32, radius: f32) {
    let x0 = ctx.commandx;
    let y0 = ctx.commandy;

    if ctx.commands.is_empty() {
        return;
    }

    if nvg__ptEquals(x0, y0, x1, y1, ctx.distTol) ||
        nvg__ptEquals(x1, y1, x2, y2, ctx.distTol) ||
        nvg__distPtSeg(x1, y1, x0, y0, x2, y2) < ctx.distTol * ctx.distTol ||
        radius < ctx.distTol {
        nvgLineTo(ctx, x1, y1);
        return;
    }

    let dx0 = x0 - x1;
    let dy0 = y0 - y1;
    let dx1 = x2 - x1;
    let dy1 = y2 - y1;
    let (dx0, dy0) = nvg__normalize(dx0, dy0);
    let (dx1, dy1) = nvg__normalize(dx1, dy1);
    let a = (dx0 * dx1 + dy0 * dy1).acos();
    let d = radius / (a / 2.0).tan();

    if d > 1.0e4 {
        nvgLineTo(ctx, x1, y1);
        return;
    }

    let cx;
    let cy;
    let a0;
    let a1;
    let dir;

    if nvg__cross(dx0, dy0, dx1, dy1) > 0.0 {
		cx = x1 + dx0 * d + dy0 * radius;
		cy = y1 + dy0 * d + (-dx0) * radius;
		a0 = dx0.atan2(-dy0);
		a1 = (-dx1).atan2(dy1);
		dir = NVG_CW;
    } else {
		cx = x1 + dx0 * d + (-dy0) * radius;
		cy = y1 + dy0 * d + dx0 * radius;
		a0 = (-dx0).atan2(dy0);
		a1 = dx1.atan2(-dy1);
		dir = NVG_CCW;
    }

    nvgArc(ctx, cx, cy, radius, a0, a1, dir);
}

pub fn nvgClosePath(ctx: &mut NVGcontext) {
    nvg__appendCommands(ctx, vec![NVGcommand::Close]);
}

pub fn nvgPathWinding(ctx: &mut NVGcontext, dir: NVGwinding) {
    nvg__appendCommands(ctx, vec![NVGcommand::Winding(dir)]);
}

pub fn nvgArc(ctx: &mut NVGcontext, cx: f32, cy: f32, r: f32, a0: f32, a1: f32, dir: NVGwinding) {
    let mut da = a1 - a0;
    if dir == NVG_CW {
        if da.abs() >= (NVG_PI * 2.0) as f32 {
            da = (NVG_PI * 2.0) as f32;
        } else {
            while da < 0.0 {
                da += (NVG_PI * 2.0) as f32;
            }
        }
    } else {
        if da.abs() >= (NVG_PI * 2.0) as f32 {
            da = (-NVG_PI * 2.0) as f32;
        } else {
            while da > 0.0 {
                da -= (NVG_PI * 2.0) as f32;
            }
        }
    }

    let ndivs = nvg__max(1, nvg__min((da.abs() / (NVG_PI * 0.5) as f32 + 0.5) as i32, 5));
    let hda = (da / ndivs as f32) / 2.0;
    let mut kappa = (4.0 / 3.0 * (1.0 - hda.cos()) / hda.sin()).abs();
    if dir == NVG_CCW {
        kappa = -kappa;
    }

    let mut px = 0.0;
    let mut py = 0.0;
    let mut ptanx = 0.0;
    let mut ptany = 0.0;
    let mut commands = Vec::with_capacity(ndivs as usize);
    for i in 0..ndivs {
        let a = a0 + da * (i as f32 / ndivs as f32);
        let dx = a.cos();
        let dy = a.sin();
        let x = cx + dx * r;
        let y = cy + dy * r;
        let tanx = -dy * r * kappa;
        let tany = dx * r * kappa;
        if i == 0 {
            if ctx.commands.is_empty() {
                commands.push(NVGcommand::MoveTo { x, y });
            } else {
                commands.push(NVGcommand::LineTo { x, y });
            }
        } else {
            commands.push(NVGcommand::BezierTo {
                c1x: px + ptanx,
                c1y: py + ptany,
                c2x: x - tanx,
                c2y: y - tany,
                x,
                y,
            });
        }
        px = x;
        py = y;
        ptanx = tanx;
        ptany = tany;
    }

    nvg__appendCommands(ctx, commands);
}

pub fn nvgRect(ctx: &mut NVGcontext, x: f32, y: f32, w: f32, h: f32) {
    nvg__appendCommands(ctx, vec![
        NVGcommand::MoveTo { x, y },
        NVGcommand::LineTo { x, y: y + h },
        NVGcommand::LineTo { x: x + w, y: y + h },
        NVGcommand::LineTo { x: x + w, y },
        NVGcommand::Close,
    ]);
}

pub fn nvgRoundedRect(ctx: &mut NVGcontext, x: f32, y: f32, w: f32, h: f32, r: f32) {
	nvgRoundedRectVarying(ctx, x, y, w, h, r, r, r, r);
}

pub fn nvgRoundedRectVarying(ctx: &mut NVGcontext, x: f32, y: f32, w: f32, h: f32,
                         radTopLeft: f32, radTopRight: f32, radBottomRight: f32,
                         radBottomLeft: f32) {
	if radTopLeft < 0.1 && radTopRight < 0.1 && radBottomRight < 0.1 && radBottomLeft < 0.1 {
		nvgRect(ctx, x, y, w, h);
		return;
	} else {
        let halfw = w.abs() * 0.5;
        let halfh = h.abs() * 0.5;
        let rxBL = nvg__min(radBottomLeft, halfw) * w.signum();
        let ryBL = nvg__min(radBottomLeft, halfh) * h.signum();
        let rxBR = nvg__min(radBottomRight, halfw) * w.signum();
        let ryBR = nvg__min(radBottomRight, halfh) * h.signum();
        let rxTR = nvg__min(radTopRight, halfw) * w.signum();
        let ryTR = nvg__min(radTopRight, halfh) * h.signum();
        let rxTL = nvg__min(radTopLeft, halfw) * w.signum();
        let ryTL = nvg__min(radTopLeft, halfh) * h.signum();
        nvg__appendCommands(ctx, vec![
            NVGcommand::MoveTo { x, y: y + ryTL },
            NVGcommand::LineTo { x, y: y + h - ryBL },
            NVGcommand::BezierTo {
                c1x: x,
                c1y: y + h - ryBL * (1.0 - NVG_KAPPA90 as f32),
                c2x: x + rxBL * (1.0 - NVG_KAPPA90 as f32),
                c2y: y + h,
                x: x + rxBL,
                y: y + h,
            },
            NVGcommand::LineTo { x: x + w - rxBR, y: y + h },
            NVGcommand::BezierTo {
                c1x: x + w - rxBR * (1.0 - NVG_KAPPA90 as f32),
                c1y: y + h,
                c2x: x + w,
                c2y: y + h - ryBR * (1.0 - NVG_KAPPA90 as f32),
                x: x + w,
                y: y + h - ryBR,
            },
            NVGcommand::LineTo { x: x + w, y: y + ryTR },
            NVGcommand::BezierTo {
                c1x: x + w,
                c1y: y + ryTR * (1.0 - NVG_KAPPA90 as f32),
                c2x: x + w - rxTR * (1.0 - NVG_KAPPA90 as f32),
                c2y: y,
                x: x + w - rxTR,
                y,
            },
            NVGcommand::LineTo { x: x + rxTL, y },
            NVGcommand::BezierTo {
                c1x: x + rxTL * (1.0 - NVG_KAPPA90 as f32),
                c1y: y,
                c2x: x,
                c2y: y + ryTL * (1.0 - NVG_KAPPA90 as f32),
                x,
                y: y + ryTL,
            },
            NVGcommand::Close,
        ]);
    }
}

pub fn nvgEllipse(ctx: &mut NVGcontext, cx: f32, cy: f32, rx: f32, ry: f32) {
    nvg__appendCommands(ctx, vec![
        NVGcommand::MoveTo { x: cx - rx, y: cy },
        NVGcommand::BezierTo {
            c1x: cx - rx,
            c1y: cy + ry * NVG_KAPPA90 as f32,
            c2x: cx - rx * NVG_KAPPA90 as f32,
            c2y: cy + ry,
            x: cx,
            y: cy + ry,
        },
        NVGcommand::BezierTo {
            c1x: cx + rx * NVG_KAPPA90 as f32,
            c1y: cy + ry,
            c2x: cx + rx,
            c2y: cy + ry * NVG_KAPPA90 as f32,
            x: cx + rx,
            y: cy,
        },
        NVGcommand::BezierTo {
            c1x: cx + rx,
            c1y: cy - ry * NVG_KAPPA90 as f32,
            c2x: cx + rx * NVG_KAPPA90 as f32,
            c2y: cy - ry,
            x: cx,
            y: cy - ry,
        },
        NVGcommand::BezierTo {
            c1x: cx - rx * NVG_KAPPA90 as f32,
            c1y: cy - ry,
            c2x: cx - rx,
            c2y: cy - ry * NVG_KAPPA90 as f32,
            x: cx - rx,
            y: cy,
        },
        NVGcommand::Close,
    ]);
}

pub fn nvgCircle(ctx: &mut NVGcontext, cx: f32, cy: f32, r: f32) {
	nvgEllipse(ctx, cx, cy, r, r);
}

pub fn nvgFill(ctx: &mut NVGcontext) {
    nvg__flattenPaths(ctx);

    if ctx.params.edgeAntiAlias != 0 && ctx.getState().shapeAntiAlias != 0 {
        let fringeWidth = ctx.fringeWidth;
        nvg__expandFill(ctx, fringeWidth, NVG_MITER, 2.4);
    } else {
        nvg__expandFill(ctx, 0.0, NVG_MITER, 2.4);
    }

    let (fillPaint, compositeOperation, scissor) = {
        let state = ctx.getState();
        let mut fillPaint = state.fill;
        fillPaint.innerColor.a *= state.alpha;
        fillPaint.outerColor.a *= state.alpha;
        let compositeOperation = state.compositeOperation;
        let scissor = state.scissor;
        (fillPaint, compositeOperation, scissor)
    };

    ctx.params.render.fill(&fillPaint, compositeOperation, &scissor, ctx.fringeWidth,
        ctx.cache.bounds, &ctx.cache.paths, &ctx.cache.verts);
}

pub fn nvgStroke(ctx: &mut NVGcontext) {
    let (strokeWidth, strokePaint, lineCap, lineJoin, miterLimit) = {
        let state = ctx.getState();
        let scale = nvg__getAverageScale(&state.xform);
        let mut strokeWidth = nvg__clamp(state.strokeWidth * scale, 0.0, 200.0);
        let mut strokePaint = state.stroke;
        if strokeWidth < ctx.fringeWidth {
            let alpha = nvg__clamp(strokeWidth / ctx.fringeWidth, 0.0, 1.0);
            strokePaint.innerColor.a *= alpha * alpha;
            strokePaint.outerColor.a *= alpha * alpha;
            strokeWidth = ctx.fringeWidth;
        }
        strokePaint.innerColor.a *= state.alpha;
        strokePaint.outerColor.a *= state.alpha;
        let lineCap = state.lineCap;
        let lineJoin = state.lineJoin;
        let miterLimit = state.miterLimit;
        (strokeWidth, strokePaint, lineCap, lineJoin, miterLimit)
    };

    nvg__flattenPaths(ctx);

    if ctx.params.edgeAntiAlias != 0 && ctx.getState().shapeAntiAlias != 0 {
        let fringeWidth = ctx.fringeWidth;
        nvg__expandStroke(ctx, strokeWidth * 0.5, fringeWidth, lineCap, lineJoin, miterLimit);
    } else {
        nvg__expandStroke(ctx, strokeWidth * 0.5, 0.0, lineCap, lineJoin, miterLimit);
    }

    let (compositeOperation, scissor) = {
        let state = ctx.getState();
        let compositeOperation = state.compositeOperation;
        let scissor = state.scissor;
        (compositeOperation, scissor)
    };

    ctx.params.render.stroke(&strokePaint, compositeOperation, &scissor, ctx.fringeWidth,
        strokeWidth, &ctx.cache.paths, &ctx.cache.verts);
}

// END: `nanovg.c`
