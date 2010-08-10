from itertools import chain
from optparse import make_option

import fbuild
import fbuild.db
import fbuild.target
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

# ------------------------------------------------------------------------------

def pre_options(parser):
    group = parser.add_option_group('config options')
    group.add_options((
        make_option('--prefix',
            default='/usr/local',
            help='specify the install location'),
        make_option('-I', '--include',
            dest='includes',
            default=[],
            action='append',
            help='Add this path to the c header search path for all phases'),
        make_option('-L', '--library-path',
            dest='libpaths',
            default=[],
            action='append',
            help='Add this path to the c library search path for all phases'),
        make_option('--c-flag',
            dest='c_flags',
            default=[],
            action='append',
            help='Add this flag to the c compiler'),
        make_option('-g', '--debug',
            default=False,
            action='store_true',
            help='enable debugging for all phases'),
        make_option('-p', '--profile',
            default=False,
            action='store_true',
            help='enable profiling for all phases'),
        make_option('-O', '--optimize',
            default=False,
            action='store_true',
            help='enable optimization for all phases'),
        make_option('--skip-tests',
            default=False,
            action='store_true',
            help='skip running tests'),
    ))

    group = parser.add_option_group('build phase options')
    group.add_options((
        make_option('--build-platform',
            help='specify the build phase platform'),
        make_option('--build-cc',
            help='specify the build phase c compiler'),
        make_option('--build-cxx',
            help='specify the build phase c++ compiler'),
        make_option('--build-include',
            dest='build_includes',
            default=[],
            action='append',
            help='Add this path to the c header search path for the build ' \
                    'phase'),
        make_option('--build-library-path',
            dest='build_libpaths',
            default=[],
            action='append',
            help='Add this path to the c library search path for the build ' \
                    'phase'),
        make_option('--build-c-flag',
            dest='build_c_flags',
            default=[],
            action='append',
            help='Add this flag to the c compiler for the build phase'),
        make_option('--build-c-debug',
            default=False,
            action='store_true',
            help='turn on c/c++ build phase debugging'),
        make_option('--build-c-profile',
            default=False,
            action='store_true',
            help='turn on c/c++ build phase profiling'),
        make_option('--build-c-optimize',
            default=False,
            action='store_true',
            help='turn on optimizations for c/c++ build phase'),
    ))

    group = parser.add_option_group('host phase options')
    group.add_options((
        make_option('--host-platform',
            help='specify the host phase platform'),
        make_option('--host-cc',
            help='specify the host phase c compiler'),
        make_option('--host-cxx',
            help='specify the host phase c++ compiler'),
        make_option('--host-include',
            dest='host_includes',
            default=[],
            action='append',
            help='Add this path to the c header search path for the host ' \
                    'phase'),
        make_option('--host-library-path',
            dest='host_libpaths',
            default=[],
            action='append',
            help='Add this path to the c library search path for the host ' \
                    'phase'),
        make_option('--host-c-flag',
            dest='host_c_flags',
            default=[],
            action='append',
            help='Add this flag to the c compiler for the host phase'),
        make_option('--host-c-debug',
            default=False,
            action='store_true',
            help='turn on c/c++ host phase debugging'),
        make_option('--host-c-profile',
            default=False,
            action='store_true',
            help='turn on c/c++ host phase profiling'),
        make_option('--host-c-optimize',
            default=False,
            action='store_true',
            help='turn on optimization for c/c++ host phase'),
        make_option('--host-ocaml-debug',
            default=False,
            action='store_true',
            help='turn on ocaml debugging'),
        make_option('--host-ocaml-profile',
            default=False,
            action='store_true',
            help='turn on ocaml profiling'),
        make_option('--host-ocamlc',
            help='specify the ocaml bytecode compiler'),
        make_option('--host-ocamlcp',
            help='specify the ocaml profiling bytecode compiler'),
        make_option('--host-ocamlopt',
            help='specify the ocaml native compiler'),
        make_option('--host-ocamllex',
            help='specify the ocaml lexer'),
        make_option('--host-use-ocamlc',
            default=False,
            action='store_true',
            help='specifically use ocamlc even if ocamlopt exists'),
        make_option('--host-llvm-config',
            help='specify the llvm-config script'),
    ))

    group = parser.add_option_group('target phase options')
    group.add_options((
        make_option('--target-platform',
            help='specify the target phase platform'),
        make_option('--target-cc',
            help='specify the target phase c compiler'),
        make_option('--target-cxx',
            help='specify the target phase c++ compiler'),
        make_option('--target-include',
            dest='target_includes',
            default=[],
            action='append',
            help='Add this path to the c header search path for the target ' \
                    'phase'),
        make_option('--target-library-path',
            dest='target_libpaths',
            default=[],
            action='append',
            help='Add this path to the c library search path for the target ' \
                    'phase'),
        make_option('--target-c-debug',
            default=False,
            action='store_true',
            help='turn on c/c++ target phase debugging'),
        make_option('--target-c-profile',
            default=False,
            action='store_true',
            help='turn on c/c++ target phase profiling'),
        make_option('--target-c-optimize',
            default=False,
            action='store_true',
            help='turn on optimization for c/c++ target phase'),
        make_option('--target-c-flag',
            dest='target_c_flags',
            default=[],
            action='append',
            help='Add this flag to the c compiler for the target phase'),
        make_option('--target-sdl-config',
            help='specify the sdl-config script'),
    ))

def post_options(options, args):
    options.prefix = Path(options.prefix)

    if options.debug:
        options.buildroot = Path(options.buildroot, 'debug')
    else:
        options.buildroot = Path(options.buildroot, 'release')

    if options.profile:
        options.buildroot += '-profile'

    if options.optimize:
        options.buildroot += '-optimized'

    return options, args

# ------------------------------------------------------------------------------

def make_c_builder(ctx, *args,
        includes=[],
        libpaths=[],
        flags=[],
        profile=False,
        **kwargs):
    flags = list(chain(ctx.options.c_flags, flags))

    # profiling is incompatible with -fomit-frame-pointer
    posix_optimize_flags = [ '-O3', '--inline']
    if not profile:
        posix_optimize_flags.append('-fomit-frame-pointer')

    kwargs['platform_options'] = [
        ({'posix'},
            {'warnings': ['all', 'fatal-errors'],
            'flags': ['-fno-common'] + flags,
            'optimize_flags': posix_optimize_flags}),
        ({'windows'}, {
            'flags': ['/GR', '/MD', '/EHs', '/wd4291'] + flags,
            'optimize_flags': ['/Ox']}),
    ]
    kwargs['includes'] = list(chain(ctx.options.includes, includes))
    kwargs['libpaths'] = list(chain(ctx.options.libpaths, libpaths))

    return Record(
        static=call('fbuild.builders.c.guess_static', ctx, *args, **kwargs),
        shared=call('fbuild.builders.c.guess_shared', ctx, *args, **kwargs))

def make_cxx_builder(ctx, *args,
        includes=[],
        libpaths=[],
        flags=[],
        profile=False,
        **kwargs):
    flags = list(chain(ctx.options.c_flags, flags))

    # profiling is incompatible with -fomit-frame-pointer
    posix_optimize_flags = [ '-O3', '--inline']
    if not profile:
        posix_optimize_flags.append('-fomit-frame-pointer')

    kwargs['platform_options'] = [
        ({'posix'}, {
            'warnings': ['all', 'fatal-errors', 'no-invalid-offsetof'],
            'flags': ['-fno-common'] + flags,
            'optimize_flags': posix_optimize_flags}),
        ({'windows'}, {
            'flags': ['/GR', '/MD', '/EHs', '/wd4291'] + flags,
            'optimize_flags': ['/Ox']}),
    ]
    kwargs['includes'] = list(chain(ctx.options.includes, includes))
    kwargs['libpaths'] = list(chain(ctx.options.libpaths, libpaths))

    return Record(
        static=call('fbuild.builders.cxx.guess_static', ctx, *args, **kwargs),
        shared=call('fbuild.builders.cxx.guess_shared', ctx, *args, **kwargs))

def config_build(ctx):
    ctx.logger.log('configuring build phase', color='cyan')

    platform = call('fbuild.builders.platform.platform', ctx,
        ctx.options.build_platform)

    return Record(
        ctx=ctx,
        platform=platform,
        c=make_c_builder(ctx, ctx.options.build_cc,
            platform=platform,
            debug=ctx.options.debug or ctx.options.build_c_debug,
            profile=ctx.options.profile or ctx.options.build_c_profile,
            optimize=ctx.options.optimize or ctx.options.build_c_optimize,
            includes=ctx.options.build_includes,
            libpaths=ctx.options.build_libpaths,
            flags=ctx.options.build_c_flags),
        cxx=make_cxx_builder(ctx, ctx.options.build_cxx,
            platform=platform,
            debug=ctx.options.debug or ctx.options.build_c_debug,
            profile=ctx.options.profile or ctx.options.build_c_profile,
            optimize=ctx.options.optimize or ctx.options.build_c_optimize,
            includes=ctx.options.build_includes,
            libpaths=ctx.options.build_libpaths,
            flags=ctx.options.build_c_flags))

def config_host(ctx, build):
    ctx.logger.log('configuring host phase', color='cyan')

    platform = call('fbuild.builders.platform.platform', ctx,
        ctx.options.build_platform)

    if platform == build.platform:
        ctx.logger.log("using build's c and cxx compiler", color='cyan')
        phase = build
    else:
        phase = Record(
            ctx=ctx,
            platform=platform,
            c=make_c_builder(ctx, fbuild.builders.host_cc,
                platform=platform,
                debug=ctx.options.debug or ctx.options.host_c_debug,
                profile=ctx.options.profile or ctx.options.host_c_profile,
                optimize=ctx.options.optimize or
                    ctx.options.host_c_optimize,
                includes=ctx.options.host_includes,
                libpaths=ctx.options.host_libpaths,
                flags=ctx.options.host_c_flags),
            cxx=make_cxx_builder(ctx, fbuild.buildesr.host_cxx,
                platform=platform,
                debug=ctx.options.debug or ctx.options.host_c_debug,
                profile=ctx.options.profile or ctx.options.host_c_profile,
                optimize=ctx.options.optimize or
                    ctx.options.host_c_optimize,
                includes=ctx.options.host_includes,
                libpaths=ctx.options.host_libpaths,
                flags=ctx.options.host_c_flags))

    ocaml = call('fbuild.builders.ocaml.ocamlfind.Ocaml', ctx,
        debug=ctx.options.debug or ctx.options.host_ocaml_debug,
        profile=ctx.options.profile or ctx.options.host_ocaml_profile,
        ocamlc=ctx.options.host_ocamlc,
        ocamlcp=ctx.options.host_ocamlcp,
        ocamlopt=ctx.options.host_ocamlopt,
        flags=['-w', 'exYz', '-warn-error', 'FDPSU'],
        requires_at_least_version=(3, 11))

    phase.ocamlc = ocaml.ocamlc
    phase.ocamlcp = ocaml.ocamlcp
    phase.ocamlopt = ocaml.ocamlopt

    phase.ocamllex = call('fbuild.builders.ocaml.Ocamllex', ctx,
        ctx.options.host_ocamllex)

    # we prefer the native ocaml as it's much faster
    if not ctx.options.host_use_ocamlc and hasattr(ocaml, 'ocamlopt'):
        phase.ocaml = phase.ocamlopt
    elif ctx.options.profile or ctx.options.host_ocaml_profile:
        phase.ocaml = phase.ocamlcp
    else:
        phase.ocaml = phase.ocamlc

    # We optionally support llvm
    try:
        llvm_config = call('fbuild.builders.llvm.LlvmConfig', ctx,
            ctx.options.host_llvm_config,
            requires_version=(2, '7svn'))
    except fbuild.ConfigFailed:
        phase.llvm_config = None
    else:
        if llvm_config.ocaml_libdir().exists():
            phase.llvm_config = llvm_config
        else:
            phase.llvm_config = None

    return phase

def config_target(ctx, host):
    ctx.logger.log('configuring target phase', color='cyan')

    platform = call('fbuild.builders.platform.platform', ctx,
        ctx.options.target_platform)

    if platform == host.platform:
        ctx.logger.log("using host's c and cxx compiler", color='cyan')
        phase = host
    else:
        phase = Record(
            ctx=ctx,
            platform=platform,
            c=make_c_builder(ctx, ctx.options.target_cc,
                platform=platform,
                debug=ctx.options.debug or ctx.options.target_c_debug,
                profile=ctx.options.profile or ctx.options.target_c_profile,
                optimize=ctx.options.optimize or
                    ctx.options.target_c_optimize,
                includes=ctx.options.target_includes,
                libpaths=ctx.options.target_libpaths,
                flags=ctx.options.target_c_flags),
            cxx=make_cxx_builder(ctx, ctx.options.target_cxx,
                platform=platform,
                debug=ctx.options.debug or ctx.options.target_c_debug,
                optimize=ctx.options.optimize or
                    ctx.options.target_c_optimize,
                includes=ctx.options.target_includes,
                libpaths=ctx.options.target_libpaths,
                flags=ctx.options.target_c_flags))

    # We optionally support sdl
    try:
        phase.sdl_config = call('fbuild.builders.sdl.SDLConfig', ctx,
            ctx.options.target_sdl_config,
            requires_at_least_version=(1, 3))
    except fbuild.ConfigFailed:
        phase.sdl_config = None

    return phase

# ------------------------------------------------------------------------------

@fbuild.db.caches
def prefix(ctx):
    prefix = Path(ctx.options.prefix)
    ctx.logger.check('install prefix', prefix, color='cyan')

    return prefix

@fbuild.db.caches
def src_dir(ctx):
    return Path(__file__).parent

# ------------------------------------------------------------------------------

def configure(ctx):
    """Configure all the phases of the compiler."""

    build = config_build(ctx)
    host = config_host(ctx, build)
    target = config_target(ctx, host)

    return Record(build=build, host=host, target=target)

def build(ctx):
    """Build the compiler and the runtime."""

    phases = configure(ctx)
    compilers = call('buildsystem.flx_compiler.build_flx_drivers', phases.host)

# ------------------------------------------------------------------------------

@fbuild.target.register()
def test(ctx):
    """Run the felix unit tests."""

    # Make sure we build felix first.
    build(ctx)

    for src in Path.glob('tests/flxi/*.flx'):
        call('buildsystem.test.test_flxi', ctx, src)
