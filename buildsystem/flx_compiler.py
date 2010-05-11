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
        external_libs=['nums'])

def build_flx_sexp(phase):
    path = Path('src', 'compiler', 'flx_sexp')
    dypgen = call('buildsystem.dypgen.build_exe', phase)
    return phase.ocaml.build_lib(path / 'flx_sexp',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[call('buildsystem.ocs.build_lib', phase)])

def build_flx_parse(phase):
    path = Path('src/compiler/flx_parse')
    dypgen = call('buildsystem.dypgen.build_exe', phase)
    return phase.ocaml.build_lib(path/'flx_parse',
        srcs=Path.globall(
            path / '*.ml{,i}',
            dypgen(path / 'flx_parse.dyp',
                flags=['--no-undef-nt', '--pv-obj', '--noemit-token-type'])),
        libs=[
            call('buildsystem.dypgen.build_lib', phase),
            call('buildsystem.ocs.build_lib', phase),
            build_flx_misc(phase),
            build_flx_core(phase),
            build_flx_sexp(phase)],
        packages=['batteries'])

def build_flx_drivers(ctx, phase):
    path = Path('src', 'compiler', 'drivers')

    flxp_libs = [
        call('buildsystem.ocs.build_lib', phase),
        call('buildsystem.dypgen.build_lib', phase),
        build_flx_parse(phase)]

    return flxp_libs
