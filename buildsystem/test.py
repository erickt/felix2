import difflib

import fbuild
import fbuild.db
from fbuild.path import Path

# ------------------------------------------------------------------------------

def test_flxi(ctx, src):
    src = Path(src)
    expect = src.replaceext('.expect')

    return check_flxi(ctx, src=src, expect=expect)

@fbuild.db.caches
def check_flxi(ctx,
        src:fbuild.db.SRC,
        expect:fbuild.db.OPTIONAL_SRC,
        env={}):
    ctx.logger.check('checking ' + src)

    # We need to read the first line to see if there are any special options.
    with open(src) as f:
        line = f.readline()

        cmd = [
            Path(ctx.buildroot) / 'bin' / 'flxi',
            '-I', Path('src') / 'lib',
            '--import', 'nugram.flxh']

        if line.startswith('// args:'):
            import json
            cmd.extend(json.loads(line.split(':', 1)[1]))

        cmd.append(src)

    # run the test.
    try:
        stdout, stderr = ctx.execute(cmd,
            timeout=60,
            quieter=1)
    except fbuild.ExecutionError as e:
        if isinstance(e, fbuild.ExecutionTimedOut):
            ctx.logger.failed('failed: timed out')
        else:
            ctx.logger.failed()

        ctx.logger.log(e, verbose=1)
        if e.stdout:
            ctx.logger.log(e.stdout.decode().strip(), verbose=1)
        if e.stderr:
            ctx.logger.log(e.stderr.decode().strip(), verbose=1)
        return False

    if expect is None:
        ctx.logger.log('no .expect', color='cyan')
        return True
    else:
        return check_expect(ctx, stdout, expect)

# ------------------------------------------------------------------------------

def check_expect(ctx, stdout, expect):
    stdout = stdout.replace(b'\r\n', b'\n').replace(b'\r', b'\n')

    with open(expect, 'rb') as f:
        s = f.read().replace(b'\r\n', b'\n').replace(b'\r', b'\n')

    if stdout == s:
        ctx.logger.passed()
        return True
    else:
        ctx.logger.failed('failed: output does not match')
        for line in difflib.ndiff(
                stdout.decode().split('\n'),
                s.decode().split('\n')):
            ctx.logger.log(line)
        return False
