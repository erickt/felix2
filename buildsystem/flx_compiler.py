import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

# ------------------------------------------------------------------------------

def build_flx_misc(phase):
    path = Path('src/compiler/flx_misc')
    return phase.ocaml.build_lib(path / 'flx_misc',
        srcs=Path.glob(path / '*.ml{,i}'),
        external_libs=['nums', 'str', 'unix'],
        packages=['batteries'])

def build_flx_core(phase):
    path = Path('src/compiler/flx_core')
    return phase.ocaml.build_lib(path / 'flx_core',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[build_flx_misc(phase)],
        external_libs=['nums'],
        packages=['batteries'])

def build_flx_parse(phase):
    path = Path('src/compiler/flx_parse')
    dypgen = call('buildsystem.dypgen.build_exe', phase)
    return phase.ocaml.build_lib(path/'flx_parse',
        srcs=Path.globall(
            path / '*.ml{,i}',
            dypgen(path / 'flx_parse.dyp',
                flags=[
                    '--no-mli',
                    '--no-undef-nt',
                    '--noemit-token-type'])),
        libs=[
            call('buildsystem.dypgen.build_lib', phase),
            call('buildsystem.ocs.build_lib', phase),
            build_flx_misc(phase),
            build_flx_core(phase)],
        packages=['batteries'])

def build_flx_bind(phase):
    path = Path('src', 'compiler', 'flx_bind')
    return phase.ocaml.build_lib(path / 'flx_bind',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            build_flx_misc(phase),
            build_flx_core(phase),
            build_flx_parse(phase)],
        packages=['batteries'])

def build_flx_codegen(phase):
    path = Path('src', 'compiler', 'flx_codegen')
    return phase.ocaml.build_lib(path / 'flx_codegen',
        srcs=Path.glob(path / '*.ml{,i}'),
        includes=[phase.llvm_config.ocaml_libdir()],
        libs=[
            build_flx_misc(phase),
            build_flx_core(phase)],
        packages=['batteries'])

def build_flx_drivers(phase):
    path = Path('src/compiler/drivers')

    call('buildsystem.ocs.build_exe', phase)

    lib = phase.ocaml.build_lib(path / 'flx_driver',
        srcs=Path.glob(path / '*.ml{,i}'))

    return {
        'flxi': phase.ocaml.build_exe('bin/flxi',
            srcs=Path(path / 'flxi' / '*.ml{,i}').glob(),
            libs=[
                call('buildsystem.dypgen.build_lib', phase),
                call('buildsystem.ocs.build_lib', phase),
                lib,
                build_flx_misc(phase),
                build_flx_core(phase),
                build_flx_parse(phase),
                build_flx_bind(phase),
                build_flx_codegen(phase)],
            external_libs=[
                'batteries',
                'threads',
                'llvm',
                'llvm_analysis',
                'llvm_executionengine',
                'llvm_scalar_opts',
                'llvm_target'],
            flags=['-thread'],
            packages=['batteries'],
            cc=phase.cxx.static.compiler.gcc.exe) }
