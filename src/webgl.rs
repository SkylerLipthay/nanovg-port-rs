// TODO: Remove
#![allow(dead_code, unused_variables)]

// Hand porting the C header where `#define NANOVG_GLES2_IMPLEMENTATION` is set.

// #define NANOVG_GLES2 1
// #define NANOVG_GL_IMPLEMENTATION 1
// #define NANOVG_GL_USE_STATE_FILTER (1)
// #define NANOVG_GL_USE_UNIFORMBUFFER 0

// TODO:
//
// Skipping text and image operations and types for now. Public:
//
// * `nvglCreateImageFromHandle`
// * `nvglImageHandle`
// * `NVGimageFlagsGL`
// * `GLNVGtexture`
//
// Private:
//
// * `GLNVGcontext.textures`, etc.
// * `GLNVG_LOC_TEX`, `tex` uniform, etc.
// * `glnvg__bindTexture`
// * `glnvg__allocTexture`
// * `glnvg__findTexture`
// * `glnvg__deleteTexture`
// * `NSVG_SHADER_FILLIMG`, `NSVG_SHADER_IMG`
//
// Not implementing these items, I think:
//
// * `nvgDelete`
//
// Hmm:
//
// * `glnvg__dumpProgramError`
// * `glnvg__checkError`
// * `nvg__fragUniformPtr`

use crate::*;
use std::{mem, slice};
use web_sys::{
    WebGlProgram, WebGlRenderingContext, WebGlShader, WebGlUniformLocation, WebGlBuffer,
};
use web_sys::WebGlRenderingContext as gl;

macro_rules! join {
    ($($part:expr,)*) => {
        {
            let mut result = String::new();
            $(result.push_str($part.to_string().as_ref());)*
            result
        }
    };

    ($($part:expr),*) => {
        join!($($part,)*)
    };
}

// START: `nanovg_gl.h` (header)

pub type NVGcreateFlags = i32;
pub const NVG_ANTIALIAS: NVGcreateFlags = 1 << 0;
pub const NVG_STENCIL_STROKES: NVGcreateFlags = 1 << 1;
pub const NVG_DEBUG: NVGcreateFlags = 1 << 2;

// END: `nanovg_gl.h` (header)

// START: `nanovg_gl.h` (implementation)

type GLNVGuniformLoc = i32;
const GLNVG_LOC_VIEWSIZE: GLNVGuniformLoc = 0;
const GLNVG_LOC_FRAG: GLNVGuniformLoc = 1;
const GLNVG_MAX_LOCS: GLNVGuniformLoc = 2;

type GLNVGshaderType = i32;
const NSVG_SHADER_FILLGRAD: GLNVGshaderType = 0;
const NSVG_SHADER_SIMPLE: GLNVGshaderType = 1;

struct GLNVGshader {
    prog: WebGlProgram,
    frag: WebGlShader,
    vert: WebGlShader,
    loc: [WebGlUniformLocation; GLNVG_MAX_LOCS as usize],
}

#[derive(Clone, Copy, Debug)]
#[repr(C)]
struct GLNVGblend {
    srcRGB: u32,
    dstRGB: u32,
    srcAlpha: u32,
    dstAlpha: u32,
}

type GLNVGcallType = i32;
const GLNVG_NONE: GLNVGcallType = 0;
const GLNVG_FILL: GLNVGcallType = 1;
const GLNVG_CONVEXFILL: GLNVGcallType = 2;
const GLNVG_STROKE: GLNVGcallType = 3;
const GLNVG_TRIANGLES: GLNVGcallType = 4;

#[derive(Clone, Copy, Debug)]
#[repr(C)]
struct GLNVGcall {
    type_: i32,
    image: i32,
    pathFirst: i32,
    pathCount: i32,
    triangleFirst: i32,
    triangleCount: i32,
    uniformIndex: i32,
    blendFunc: GLNVGblend,
}

#[derive(Clone, Copy, Debug)]
#[repr(C)]
struct GLNVGpath {
    fillFirst: i32,
    fillCount: i32,
    strokeFirst: i32,
    strokeCount: i32,
}

#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
struct GLNVGfragUniforms {
    scissorMat: [f32; 12],
    paintMat: [f32; 12],
    innerCol: NVGcolor,
    outerCol: NVGcolor,
    scissorExt: [f32; 2],
    scissorScale: [f32; 2],
    extent: [f32; 2],
    radius: f32,
    feather: f32,
    strokeMult: f32,
    strokeThr: f32,
    texType: f32, // Unused currently.
    type_: f32,
}

impl GLNVGfragUniforms {
    fn arr(&mut self) -> &mut [f32] {
        unsafe {
            let data = mem::transmute::<&mut GLNVGfragUniforms, *mut f32>(self);
            slice::from_raw_parts_mut(data, UNIFORMS_VEC_SIZE * F32_SIZE)
        }
    }
}

struct GLNVGcontext {
    gl: WebGlRenderingContext,
    shader: GLNVGshader,
    view: [f32; 2],
    vertBuf: WebGlBuffer,
    fragSize: i32,
    flags: NVGcreateFlags,
    calls: Vec<GLNVGcall>,
    paths: Vec<GLNVGpath>,
    verts: Vec<NVGvertex>,
    uniforms: Vec<GLNVGfragUniforms>,
    stencilMask: u32,
    stencilFunc: u32,
    stencilFuncRef: i32,
    stencilFuncMask: u32,
    blendFunc: GLNVGblend,
}

fn verts_arr(verts: &mut Vec<NVGvertex>) -> &mut [u8] {
    unsafe {
        let data = mem::transmute::<*mut NVGvertex, *mut u8>(verts.as_mut_ptr());
        slice::from_raw_parts_mut(data, mem::size_of::<NVGvertex>() * verts.len())
    }
}

const F32_SIZE: usize = 4;
const UNIFORM_SIZE: usize = 4;
const UNIFORMS_SIZE: usize = mem::size_of::<GLNVGfragUniforms>();
const UNIFORMS_VEC_SIZE: usize = UNIFORMS_SIZE / UNIFORM_SIZE / F32_SIZE;
const_assert!(uniforms_size; UNIFORMS_SIZE == (UNIFORMS_VEC_SIZE * UNIFORM_SIZE * F32_SIZE));
const_assert!(uniforms_vec_size; UNIFORMS_VEC_SIZE == 11);

fn glnvg__nearestPow2(num: u32) -> u32 {
    let mut n = if num > 0 { num - 1 } else { 0 };
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
    n += 1;
    n
}

fn glnvg__stencilMask(ctx: &mut GLNVGcontext, mask: u32) {
    if ctx.stencilMask != mask {
        ctx.stencilMask = mask;
        ctx.gl.stencil_mask(mask);
    }
}

fn glnvg__stencilFunc(ctx: &mut GLNVGcontext, func: u32, ref_: i32, mask: u32) {
    if ctx.stencilFunc != func || ctx.stencilFuncRef != ref_ || ctx.stencilFuncMask != mask {
        ctx.stencilFunc = func;
        ctx.stencilFuncRef = ref_;
        ctx.stencilFuncMask = mask;
        ctx.gl.stencil_func(func, ref_, mask);
    }
}

fn glnvg__blendFuncSeparate(ctx: &mut GLNVGcontext, blend: &GLNVGblend) {
	if ctx.blendFunc.srcRGB != blend.srcRGB || ctx.blendFunc.dstRGB != blend.dstRGB ||
        ctx.blendFunc.srcAlpha != blend.srcAlpha || ctx.blendFunc.dstAlpha != blend.dstAlpha
    {
		ctx.blendFunc = *blend;
		ctx.gl.blend_func_separate(blend.srcRGB, blend.dstRGB, blend.srcAlpha, blend.dstAlpha);
	}
}

fn glnvg__createShader(
    gl: &WebGlRenderingContext,
    header: &str,
    vshader: &str,
    fshader: &str,
) -> Result<GLNVGshader, ()> {
    let prog = match gl.create_program() {
        Some(prog) => prog,
        None => return Err(()),
    };

    let vert = match gl.create_shader(gl::VERTEX_SHADER) {
        Some(shader) => shader,
        None => return Err(()),
    };

    let frag = match gl.create_shader(gl::FRAGMENT_SHADER) {
        Some(shader) => shader,
        None => return Err(()),
    };

    gl.shader_source(&vert, join!(header, vshader).as_str());
    gl.shader_source(&frag, join!(header, fshader).as_str());

    gl.compile_shader(&vert);
    if gl.get_shader_parameter(&vert, gl::COMPILE_STATUS) != true {
        return Err(());
    }

    gl.compile_shader(&frag);
    if gl.get_shader_parameter(&frag, gl::COMPILE_STATUS) != true {
        return Err(());
    }

    gl.attach_shader(&prog, &vert);
    gl.attach_shader(&prog, &frag);
    gl.bind_attrib_location(&prog, 0, "vertex");
    gl.bind_attrib_location(&prog, 1, "tcoord");

    gl.link_program(&prog);
    if gl.get_program_parameter(&prog, gl::LINK_STATUS) != true {
        return Err(());
    }

    let locViewSize = match gl.get_uniform_location(&prog, "viewSize") {
        Some(l) => l,
        None => return Err(()),
    };

    let locFrag = match gl.get_uniform_location(&prog, "frag") {
        Some(l) => l,
        None => return Err(()),
    };

    let loc = [locViewSize, locFrag];
    Ok(GLNVGshader { prog, frag, vert, loc })
}

pub fn nvgCreate(gl: WebGlRenderingContext, flags: NVGcreateFlags) -> Result<NVGcontext, ()> {
    let mut header = join!("#version 100\n#define UNIFORMS_VEC_SIZE ", UNIFORMS_VEC_SIZE, "\n");
    if flags & NVG_ANTIALIAS != 0 {
        header = join!(header, "#define EDGE_AA 1\n");
    }
    let shader = glnvg__createShader(&gl, header.as_str(), SHADER_VERTEX, SHADER_FRAGMENT)?;

    let vertBuf = match gl.create_buffer() {
        Some(buf) => buf,
        None => return Err(()),
    };

    let memSize = mem::size_of::<GLNVGfragUniforms>();
    let fragSize = (memSize + 4 - memSize % 4) as i32;

    gl.finish();

    Ok(nvgCreateInternal(NVGparams {
        edgeAntiAlias: if flags & NVG_ANTIALIAS != 0 { 1 } else { 1 },
        render: Box::new(GLNVGcontext {
            gl,
            shader,
            view: [0.0, 0.0],
            vertBuf,
            fragSize,
            flags,
            calls: Vec::new(),
            paths: Vec::new(),
            verts: Vec::new(),
            uniforms: Vec::new(),
            stencilMask: 0,
            stencilFunc: 0,
            stencilFuncRef: 0,
            stencilFuncMask: 0,
            blendFunc: GLNVGblend {
                srcRGB: 0,
                dstRGB: 0,
                srcAlpha: 0,
                dstAlpha: 0,
            },
        }),
    }))
}

#[inline]
fn glnvg__xformToMat3x4(t: &NVGtransform) -> [f32; 12] {
    [t[0], t[1], 0.0, 0.0, t[2], t[3], 0.0, 0.0, t[4], t[5], 1.0, 0.0]
}

fn glnvg__premulColor(mut c: NVGcolor) -> NVGcolor {
	c.r *= c.a;
	c.g *= c.a;
	c.b *= c.a;
	c
}

fn glnvg__convertPaint(
    ctx: &GLNVGcontext,
    paint: &NVGpaint,
    scissor: &NVGscissor,
    width: f32,
    fringe: f32,
    strokeThr: f32,
) -> GLNVGfragUniforms {
    let innerCol = glnvg__premulColor(paint.innerColor);
    let outerCol = glnvg__premulColor(paint.outerColor);

    let scissorMat;
    let scissorExt;
    let scissorScale;

    if scissor.extent[0] < -0.5 || scissor.extent[1] < -0.5 {
        scissorMat = [0.0; 12];
        scissorExt = [1.0, 1.0];
        scissorScale = [1.0, 1.0];
    } else {
        let xform = &scissor.xform;
        let invxform = nvgTransformInverse(*xform).unwrap_or_else(|| nvgTransformIdentity());
        scissorMat = glnvg__xformToMat3x4(&invxform);
        scissorExt = scissor.extent;
        scissorScale = [
            (xform[0] * xform[0] + xform[2] * xform[2]).sqrt() / fringe,
            (xform[1] * xform[1] + xform[3] * xform[3]).sqrt() / fringe,
        ];
    }

    let extent = paint.extent;
    let strokeMult = (width * 0.5 + fringe * 0.5) / fringe;
    let strokeThr = strokeThr;

    // TODO: paint->image

    let type_ = NSVG_SHADER_FILLGRAD as f32;
    let radius = paint.radius;
    let feather = paint.feather;
    let xform = paint.xform;
    let invxform = nvgTransformInverse(xform).unwrap_or_else(|| nvgTransformIdentity());
    let paintMat = glnvg__xformToMat3x4(&invxform);

    GLNVGfragUniforms {
        scissorMat,
        paintMat,
        innerCol,
        outerCol,
        scissorExt,
        scissorScale,
        extent,
        radius,
        feather,
        strokeMult,
        strokeThr,
        texType: 0.0, // TODO
        type_,
    }
}

fn glnvg__setUniforms(ctx: &mut GLNVGcontext, uniformIndex: i32 /*, TODO: image: i32*/) {
    let uniform = &mut ctx.uniforms[uniformIndex as usize];
    let arr = uniform.arr();
    ctx.gl.uniform4fv_with_f32_array(Some(&ctx.shader.loc[GLNVG_LOC_FRAG as usize]), arr);
    // TODO: This is shaky stuff here, double check it:
    // ::web_sys::console::log_1(&(arr.len() as u32).into());
}

fn glnvg__renderViewport(ctx: &mut GLNVGcontext, width: f32, height: f32) {
    ctx.view[0] = width;
    ctx.view[1] = height;
}

fn glnvg__fill(ctx: &mut GLNVGcontext, call: &GLNVGcall) {
    let first = call.pathFirst as usize;
    let count = call.pathCount as usize;

    ctx.gl.enable(gl::STENCIL_TEST);
	glnvg__stencilMask(ctx, 0xff);
	glnvg__stencilFunc(ctx, gl::ALWAYS, 0, 0xff);
    ctx.gl.color_mask(false, false, false, false);

    glnvg__setUniforms(ctx, call.uniformIndex);

    ctx.gl.stencil_op_separate(gl::FRONT, gl::KEEP, gl::KEEP, gl::INCR_WRAP);
    ctx.gl.stencil_op_separate(gl::BACK, gl::KEEP, gl::KEEP, gl::DECR_WRAP);
    ctx.gl.disable(gl::CULL_FACE);
    for path in &ctx.paths[first..(first + count)] {
        ctx.gl.draw_arrays(gl::TRIANGLE_FAN, path.fillFirst, path.fillCount);
    }
    ctx.gl.enable(gl::CULL_FACE);

    ctx.gl.color_mask(true, true, true, true);

    glnvg__setUniforms(ctx, call.uniformIndex + 1);

    if ctx.flags & NVG_ANTIALIAS != 0 {
        glnvg__stencilFunc(ctx, gl::EQUAL, 0x00, 0xff);
        ctx.gl.stencil_op(gl::KEEP, gl::KEEP, gl::KEEP);
        for path in &ctx.paths[first..(first + count)] {
            ctx.gl.draw_arrays(gl::TRIANGLE_STRIP, path.strokeFirst, path.strokeCount);
        }
    }

    glnvg__stencilFunc(ctx, gl::NOTEQUAL, 0x0, 0xff);
    ctx.gl.stencil_op(gl::ZERO, gl::ZERO, gl::ZERO);
    ctx.gl.draw_arrays(gl::TRIANGLE_STRIP, call.triangleFirst, call.triangleCount);

    ctx.gl.disable(gl::STENCIL_TEST);
}

fn glnvg__convexFill(ctx: &mut GLNVGcontext, call: &GLNVGcall) {
	glnvg__setUniforms(ctx, call.uniformIndex);

    let first = call.pathFirst as usize;
    let count = call.pathCount as usize;
    let paths = &ctx.paths[first..(first + count)];

    for path in paths {
        ctx.gl.draw_arrays(gl::TRIANGLE_FAN, path.fillFirst, path.fillCount);
    }

    if ctx.flags & NVG_ANTIALIAS != 0 {
        for path in paths {
            ctx.gl.draw_arrays(gl::TRIANGLE_STRIP, path.strokeFirst, path.strokeCount);
        }
    }
}

fn glnvg__stroke(ctx: &mut GLNVGcontext, call: &GLNVGcall) {
    let first = call.pathFirst as usize;
    let count = call.pathCount as usize;

    if ctx.flags & NVG_STENCIL_STROKES != 0 {
        ctx.gl.enable(gl::STENCIL_TEST);
        glnvg__stencilMask(ctx, 0xff);
        glnvg__stencilFunc(ctx, gl::EQUAL, 0, 0xff);
        ctx.gl.stencil_op(gl::KEEP, gl::KEEP, gl::INCR);
        glnvg__setUniforms(ctx, call.uniformIndex + 1);
        for path in &ctx.paths[first..(first + count)] {
            ctx.gl.draw_arrays(gl::TRIANGLE_STRIP, path.strokeFirst, path.strokeCount);
        }

        glnvg__setUniforms(ctx, call.uniformIndex);
        glnvg__stencilFunc(ctx, gl::EQUAL, 0x00, 0xff);
        ctx.gl.stencil_op(gl::KEEP, gl::KEEP, gl::KEEP);
        for path in &ctx.paths[first..(first + count)] {
            ctx.gl.draw_arrays(gl::TRIANGLE_STRIP, path.strokeFirst, path.strokeCount);
        }

        ctx.gl.color_mask(false, false, false, false);
        glnvg__stencilFunc(ctx, gl::ALWAYS, 0x0, 0xff);
        ctx.gl.stencil_op(gl::ZERO, gl::ZERO, gl::ZERO);
        for path in &ctx.paths[first..(first + count)] {
            ctx.gl.draw_arrays(gl::TRIANGLE_STRIP, path.strokeFirst, path.strokeCount);
        }
        ctx.gl.color_mask(true, true, true, true);

        ctx.gl.disable(gl::STENCIL_TEST);
    } else {
        glnvg__setUniforms(ctx, call.uniformIndex);
        for path in &ctx.paths[first..(first + count)] {
            ctx.gl.draw_arrays(gl::TRIANGLE_STRIP, path.strokeFirst, path.strokeCount);
        }
    }
}

fn glnvg__triangles(ctx: &mut GLNVGcontext, call: &GLNVGcall) {
    glnvg__setUniforms(ctx, call.uniformIndex);
    ctx.gl.draw_arrays(gl::TRIANGLES, call.triangleFirst, call.triangleCount);
}

fn glnvg__renderCancel(ctx: &mut GLNVGcontext) {
    ctx.verts.clear();
    ctx.paths.clear();
    ctx.calls.clear();
    ctx.uniforms.clear();
}

fn glnvg_convertBlendFuncFactor(factor: NVGblendFactor) -> u32 {
    match factor {
        NVG_ZERO => gl::ZERO,
        NVG_ONE => gl::ONE,
        NVG_SRC_COLOR => gl::SRC_COLOR,
        NVG_ONE_MINUS_SRC_COLOR => gl::ONE_MINUS_SRC_COLOR,
        NVG_DST_COLOR => gl::DST_COLOR,
        NVG_ONE_MINUS_DST_COLOR => gl::ONE_MINUS_DST_COLOR,
        NVG_SRC_ALPHA => gl::SRC_ALPHA,
        NVG_ONE_MINUS_SRC_ALPHA => gl::ONE_MINUS_SRC_ALPHA,
        NVG_DST_ALPHA => gl::DST_ALPHA,
        NVG_ONE_MINUS_DST_ALPHA => gl::ONE_MINUS_DST_ALPHA,
        NVG_SRC_ALPHA_SATURATE => gl::SRC_ALPHA_SATURATE,
        _ => gl::INVALID_ENUM,
    }
}

fn glnvg__blendCompositeOperation(op: NVGcompositeOperationState) -> GLNVGblend {
    let blend = GLNVGblend {
        srcRGB: glnvg_convertBlendFuncFactor(op.srcRGB),
        dstRGB: glnvg_convertBlendFuncFactor(op.dstRGB),
        srcAlpha: glnvg_convertBlendFuncFactor(op.srcAlpha),
        dstAlpha: glnvg_convertBlendFuncFactor(op.dstAlpha),
    };

    if blend.srcRGB == gl::INVALID_ENUM ||
        blend.dstRGB == gl::INVALID_ENUM ||
        blend.srcAlpha == gl::INVALID_ENUM ||
        blend.dstAlpha == gl::INVALID_ENUM
    {
        return GLNVGblend {
            srcRGB: gl::ONE,
            dstRGB: gl::ONE_MINUS_SRC_ALPHA,
            srcAlpha: gl::ONE,
            dstAlpha: gl::ONE_MINUS_SRC_ALPHA,
        };
    }

    blend
}

fn glnvg__renderFlush(ctx: &mut GLNVGcontext) {
    if !ctx.calls.is_empty() {
        ctx.gl.use_program(Some(&ctx.shader.prog));
        ctx.gl.enable(gl::CULL_FACE);
        ctx.gl.cull_face(gl::BACK);
        ctx.gl.front_face(gl::CCW);
        ctx.gl.enable(gl::BLEND);
        ctx.gl.disable(gl::DEPTH_TEST);
        ctx.gl.disable(gl::SCISSOR_TEST);
        ctx.gl.color_mask(true, true, true, true);
        ctx.gl.stencil_mask(0xffffffff);
        ctx.gl.stencil_op(gl::KEEP, gl::KEEP, gl::KEEP);
        ctx.gl.stencil_func(gl::ALWAYS, 0, 0xffffffff);
        // TODO: ctx.gl.active_texture(gl::TEXTURE0);
        // TODO: ctx.gl.bind_texture(gl::TEXTURE_2D, 0);
        // TODO: ctx.boundTexture = 0;
        ctx.stencilMask = 0xffffffff;
        ctx.stencilFunc = gl::ALWAYS;
        ctx.stencilFuncRef = 0;
        ctx.stencilFuncMask = 0xffffffff;
        ctx.blendFunc.srcRGB = gl::INVALID_ENUM;
        ctx.blendFunc.srcAlpha = gl::INVALID_ENUM;
        ctx.blendFunc.dstRGB = gl::INVALID_ENUM;
        ctx.blendFunc.dstAlpha = gl::INVALID_ENUM;

        ctx.gl.bind_buffer(gl::ARRAY_BUFFER, Some(&ctx.vertBuf));
        ctx.gl.buffer_data_with_u8_array(gl::ARRAY_BUFFER, verts_arr(&mut ctx.verts),
            gl::STREAM_DRAW);
        ctx.gl.enable_vertex_attrib_array(0);
        ctx.gl.enable_vertex_attrib_array(1);
        ctx.gl.vertex_attrib_pointer_with_i32(0, 2, gl::FLOAT, false,
            mem::size_of::<NVGvertex>() as i32, 0);
        ctx.gl.vertex_attrib_pointer_with_i32(1, 2, gl::FLOAT, false,
            mem::size_of::<NVGvertex>() as i32, (2 * mem::size_of::<f32>()) as i32);

        // TODO: GLNVG_LOC_TEX
        ctx.gl.uniform2fv_with_f32_array(
            Some(&ctx.shader.loc[GLNVG_LOC_VIEWSIZE as usize]),
            &mut ctx.view,
        );

        // We need move `calls` out of `ctx` temporarily, to appease the borrow checker.
        let calls = mem::replace(&mut ctx.calls, Vec::new());
        for call in calls.iter() {
            glnvg__blendFuncSeparate(ctx, &call.blendFunc);
            match call.type_ {
                GLNVG_FILL => glnvg__fill(ctx, call),
                GLNVG_CONVEXFILL => glnvg__convexFill(ctx, call),
                GLNVG_STROKE => glnvg__stroke(ctx, call),
                GLNVG_TRIANGLES => glnvg__triangles(ctx, call),
                _ => { },
            }
        }
        ctx.calls = calls;

        ctx.gl.disable_vertex_attrib_array(0);
        ctx.gl.disable_vertex_attrib_array(1);
        ctx.gl.disable(gl::CULL_FACE);
        ctx.gl.bind_buffer(gl::ARRAY_BUFFER, None);
        ctx.gl.use_program(None);
        // TODO: glnvg__bindTexture(ctx, 0);
    }

    ctx.verts.clear();
    ctx.paths.clear();
    ctx.calls.clear();
    ctx.uniforms.clear();
}

fn glnvg__maxVertCount(paths: &[NVGpath]) -> i32 {
    paths.iter().fold(0, |acc, p| acc + p.fillCount + p.strokeCount)
}

fn glnvg__renderFill(
    ctx: &mut GLNVGcontext,
    paint: &NVGpaint,
    compositeOperation: NVGcompositeOperationState,
    scissor: &NVGscissor,
    fringe: f32,
    bounds: [f32; 4],
    paths: &[NVGpath],
    verts: &[NVGvertex],
) {
    let mut type_ = GLNVG_FILL;
	let mut triangleCount = 4;
	let pathFirst = ctx.paths.len();
	let pathCount = paths.len();
	let image = 0; // TODO
	let blendFunc = glnvg__blendCompositeOperation(compositeOperation);

    reserve!(ctx.paths, pathCount);

    if paths.len() == 1 && paths[0].convex != 0 {
        type_ = GLNVG_CONVEXFILL;
        triangleCount = 0;
    }

    let cverts = glnvg__maxVertCount(paths) + triangleCount;
    reserve!(ctx.verts, cverts);

    let mut offset = ctx.verts.len();

    for path in paths {
        let mut fillFirst = 0;
        let mut fillCount = 0;
        let mut strokeFirst = 0;
        let mut strokeCount = 0;
        if path.fillCount > 0 {
            fillFirst = offset as i32;
            fillCount = path.fillCount;
            let vstart = path.fillFirst;
            let vend = path.fillFirst + path.fillCount as usize;
            ctx.verts.extend_from_slice(&verts[vstart..vend]);
            offset += path.fillCount as usize;
        }
        if path.strokeCount > 0 {
            strokeFirst = offset as i32;
            strokeCount = path.strokeCount;
            let vstart = path.strokeFirst;
            let vend = path.strokeFirst + path.strokeCount as usize;
            ctx.verts.extend_from_slice(&verts[vstart..vend]);
            offset += path.strokeCount as usize;
        }
        ctx.paths.push(GLNVGpath { fillFirst, fillCount, strokeFirst, strokeCount });
    }

    let uniformIndex = ctx.uniforms.len() as i32;
    let triangleFirst;

    if type_ == GLNVG_FILL {
        triangleFirst = offset;
        ctx.verts.push(NVGvertex { x: bounds[2], y: bounds[3], u: 0.5, v: 1.0 });
        ctx.verts.push(NVGvertex { x: bounds[2], y: bounds[1], u: 0.5, v: 1.0 });
        ctx.verts.push(NVGvertex { x: bounds[0], y: bounds[3], u: 0.5, v: 1.0 });
        ctx.verts.push(NVGvertex { x: bounds[0], y: bounds[1], u: 0.5, v: 1.0 });
        ctx.uniforms.push(GLNVGfragUniforms {
            strokeThr: -1.0,
            type_: NSVG_SHADER_SIMPLE as f32,
            ..Default::default()
        });
        let uniform = glnvg__convertPaint(&ctx, paint, scissor, fringe, fringe, -1.0);
        ctx.uniforms.push(uniform);
    } else {
        triangleFirst = 0;
        let uniform = glnvg__convertPaint(&ctx, paint, scissor, fringe, fringe, -1.0);
        ctx.uniforms.push(uniform);
    }

    ctx.calls.push(GLNVGcall {
        type_,
        image,
        pathFirst: pathFirst as i32,
        pathCount: pathCount as i32,
        triangleFirst: triangleFirst as i32,
        triangleCount,
        uniformIndex,
        blendFunc,
    });
}

// TODO: glnvg__renderStroke
// TODO: glnvg__renderTriangles

// static void glnvg__renderFill(void* uptr, NVGpaint* paint, NVGcompositeOperationState compositeOperation, NVGscissor* scissor, float fringe,
// 							  const float* bounds, const NVGpath* paths, int npaths)
// {
// 	GLNVGcontext* gl = (GLNVGcontext*)uptr;
// 	GLNVGcall* call = glnvg__allocCall(gl);
// 	NVGvertex* quad;
// 	GLNVGfragUniforms* frag;
// 	int i, maxverts, offset;

// 	if (call == NULL) return;

// 	call->type = GLNVG_FILL;
// 	call->triangleCount = 4;
// 	call->pathOffset = glnvg__allocPaths(gl, npaths);
// 	if (call->pathOffset == -1) goto error;
// 	call->pathCount = npaths;
// 	call->image = paint->image;
// 	call->blendFunc = glnvg__blendCompositeOperation(compositeOperation);

// 	if (npaths == 1 && paths[0].convex)
// 	{
// 		call->type = GLNVG_CONVEXFILL;
// 		call->triangleCount = 0;	// Bounding box fill quad not needed for convex fill
// 	}

// 	// Allocate vertices for all the paths.
// 	maxverts = glnvg__maxVertCount(paths, npaths) + call->triangleCount;
// 	offset = glnvg__allocVerts(gl, maxverts);
// 	if (offset == -1) goto error;

// 	for (i = 0; i < npaths; i++) {
// 		GLNVGpath* copy = &gl->paths[call->pathOffset + i];
// 		const NVGpath* path = &paths[i];
// 		memset(copy, 0, sizeof(GLNVGpath));
// 		if (path->nfill > 0) {
// 			copy->fillOffset = offset;
// 			copy->fillCount = path->nfill;
// 			memcpy(&gl->verts[offset], path->fill, sizeof(NVGvertex) * path->nfill);
// 			offset += path->nfill;
// 		}
// 		if (path->nstroke > 0) {
// 			copy->strokeOffset = offset;
// 			copy->strokeCount = path->nstroke;
// 			memcpy(&gl->verts[offset], path->stroke, sizeof(NVGvertex) * path->nstroke);
// 			offset += path->nstroke;
// 		}
// 	}

// 	// Setup uniforms for draw calls
// 	if (call->type == GLNVG_FILL) {
// 		// Quad
// 		call->triangleOffset = offset;
// 		quad = &gl->verts[call->triangleOffset];
// 		glnvg__vset(&quad[0], bounds[2], bounds[3], 0.5f, 1.0f);
// 		glnvg__vset(&quad[1], bounds[2], bounds[1], 0.5f, 1.0f);
// 		glnvg__vset(&quad[2], bounds[0], bounds[3], 0.5f, 1.0f);
// 		glnvg__vset(&quad[3], bounds[0], bounds[1], 0.5f, 1.0f);

// 		call->uniformOffset = glnvg__allocFragUniforms(gl, 2);
// 		if (call->uniformOffset == -1) goto error;
// 		// Simple shader for stencil
// 		frag = nvg__fragUniformPtr(gl, call->uniformOffset);
// 		memset(frag, 0, sizeof(*frag));
// 		frag->strokeThr = -1.0f;
// 		frag->type = NSVG_SHADER_SIMPLE;
// 		// Fill shader
// 		glnvg__convertPaint(gl, nvg__fragUniformPtr(gl, call->uniformOffset + gl->fragSize), paint, scissor, fringe, fringe, -1.0f);
// 	} else {
// 		call->uniformOffset = glnvg__allocFragUniforms(gl, 1);
// 		if (call->uniformOffset == -1) goto error;
// 		// Fill shader
// 		glnvg__convertPaint(gl, nvg__fragUniformPtr(gl, call->uniformOffset), paint, scissor, fringe, fringe, -1.0f);
// 	}

// 	return;

// error:
// 	// We get here if call alloc was ok, but something else is not.
// 	// Roll back the last call to prevent drawing it.
// 	if (gl->ncalls > 0) gl->ncalls--;
// }

impl NVGrender for GLNVGcontext {
    fn viewport(&mut self, width: f32, height: f32, devicePixelRatio: f32) {
        glnvg__renderViewport(self, width, height);
    }

    fn cancel(&mut self) {
        glnvg__renderCancel(self);
    }

    fn flush(&mut self) {
        glnvg__renderFlush(self);
    }

    fn fill(&mut self, paint: &NVGpaint, compositeOperation: NVGcompositeOperationState,
            scissor: &NVGscissor, fringe: f32, bounds: [f32; 4], paths: &[NVGpath],
            verts: &[NVGvertex]) {
        glnvg__renderFill(self, paint, compositeOperation, scissor, fringe, bounds, paths, verts);
    }

    fn stroke(&mut self, paint: &NVGpaint, compositeOperation: NVGcompositeOperationState,
              scissor: &NVGscissor, fringe: f32, strokeWidth: f32, paths: &[NVGpath],
              verts: &[NVGvertex]) {
    }
}

impl Drop for GLNVGcontext {
    fn drop(&mut self) {
        self.gl.delete_program(Some(&self.shader.prog));
        self.gl.delete_shader(Some(&self.shader.vert));
        self.gl.delete_shader(Some(&self.shader.frag));
        self.gl.delete_buffer(Some(&self.vertBuf));
    }
}

const SHADER_VERTEX: &str = "uniform vec2 viewSize;
attribute vec2 vertex;
attribute vec2 tcoord;
varying vec2 ftcoord;
varying vec2 fpos;

void main(void) {
    ftcoord = tcoord;
    fpos = vertex;
    gl_Position = vec4(
        2.0 * vertex.x / viewSize.x - 1.0,
        1.0 - 2.0 * vertex.y / viewSize.y,
        0,
        1
    );
}";

const SHADER_FRAGMENT: &str = "precision mediump float;

uniform vec4 frag[UNIFORMS_VEC_SIZE];
varying vec2 ftcoord;
varying vec2 fpos;

#define scissorMat mat3(frag[0].xyz, frag[1].xyz, frag[2].xyz)
#define paintMat mat3(frag[3].xyz, frag[4].xyz, frag[5].xyz)
#define innerCol frag[6]
#define outerCol frag[7]
#define scissorExt frag[8].xy
#define scissorScale frag[8].zw
#define extent frag[9].xy
#define radius frag[9].z
#define feather frag[9].w
#define strokeMult frag[10].x
#define strokeThr frag[10].y
#define texType int(frag[10].z)
#define type_ int(frag[10].w)

float sdroundrect(vec2 pt, vec2 ext, float rad) {
    vec2 ext2 = ext - vec2(rad, rad);
    vec2 d = abs(pt) - ext2;
    return min(max(d.x, d.y), 0.0) + length(max(d, 0.0)) - rad;
}

// Scissoring
float scissorMask(vec2 p) {
    vec2 sc = (abs((scissorMat * vec3(p, 1.0)).xy) - scissorExt);
    sc = vec2(0.5, 0.5) - sc * scissorScale;
    return clamp(sc.x, 0.0, 1.0) * clamp(sc.y, 0.0, 1.0);
}

#ifdef EDGE_AA
// Stroke - from [0..1] to clipped pyramid, where the slope is 1px.
float strokeMask() {
    return min(1.0, (1.0 - abs(ftcoord.x * 2.0 - 1.0)) * strokeMult) * min(1.0, ftcoord.y);
}
#endif

void main(void) {
    vec4 result;
    float scissor = scissorMask(fpos);

    #ifdef EDGE_AA
    float strokeAlpha = strokeMask();
    if (strokeAlpha < strokeThr) discard;
    #else
    float strokeAlpha = 1.0;
    #endif

    if (type_ == 0) { // Gradient
        // Calculate gradient color using box gradient.
        vec2 pt = (paintMat * vec3(fpos, 1.0)).xy;
        float d = clamp((sdroundrect(pt, extent, radius) + feather * 0.5) / feather, 0.0, 1.0);
        vec4 color = mix(innerCol, outerCol, d);
        // Combine alpha:
        color *= strokeAlpha * scissor;
        result = color;
    } else if (type_ == 1) { // Stencil fill
        result = vec4(1, 1, 1, 1);
    }

    gl_FragColor = result;
}";

// END: `nanovg_gl.h` (implementation)
