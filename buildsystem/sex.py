import fbuild
from fbuild.functools import call
from fbuild.path import Path

# ------------------------------------------------------------------------------

def build(phase):
    path = Path('src', 'compiler', 'sex')
    dypgen = call('buildsystem.dypgen.build_exe', phase)
    return phase.ocaml.build_lib(path/'sex', Path.glob(path/'*.ml{,i}'),
        libs=[
            call('buildsystem.ocs.build_lib', phase),
            call('buildsystem.dypgen.build_lib', phase)])
