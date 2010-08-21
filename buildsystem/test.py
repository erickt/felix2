import time
import difflib

import fbuild
import fbuild.db
from fbuild.path import Path

# ------------------------------------------------------------------------------

def _make_stdout_dst(ctx, src):
    """Builds the dst file."""
    return src.addroot(ctx.buildroot).replaceext('.stdout')

def test_flxi(ctx, srcs):
    """Run flxi specific tests and return which srcs failed to run.."""

    def run_test(src):
        src = Path(src)
        dst = _make_stdout_dst(ctx, src)
        expect = src.replaceext('.expect')

        # Make sure the destination directory exists.
        dst.parent.makedirs()

        if not expect.exists():
            expect = None

        passed = check_flxi_test(ctx, src=src, dst=dst, expect=expect)
        return src, passed

    failed_srcs = []
    for src, passed in ctx.scheduler.map(run_test, srcs):
        if not passed:
            failed_srcs.append(src)

    return failed_srcs

def clean_test_temporary_files(ctx, srcs):
    """Delete's the test temporary files."""

    def clean(src):
        src = Path(src)
        dst = _make_stdout_dst(ctx, src)

        if dst.exists():
            ctx.logger.log('removing ' + dst)
            dst.remove()

    ctx.scheduler.map(clean, srcs)

def check_flxi_test(ctx, src, dst, expect, env={}):
    """Run the test and return whether or not it equals the expected
    results."""

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

    with open(dst, 'wb') as f:
        f.write(stdout)

    if expect is None:
        ctx.logger.log('no .expect', color='cyan')
        return True
    else:
        stdout = stdout.replace(b'\r\n', b'\n').replace(b'\r', b'\n')

        with open(expect, 'rb') as f:
            s = f.read().replace(b'\r\n', b'\n').replace(b'\r', b'\n')

        if stdout == s:
            ctx.logger.passed()
            return True
        else:
            ctx.logger.failed('failed: output does not match')
            max_filename = max(len(expect), len(dst))
            for line in difflib.unified_diff(
                    stdout.decode().split('\n'),
                    s.decode().split('\n'),
                    fromfile=expect.ljust(max_filename),
                    tofile=dst.ljust(max_filename),
                    fromfiledate=time.ctime(expect.getmtime()),
                    tofiledate=time.ctime(dst.getmtime()),
                    lineterm=''):
                ctx.logger.log(line)
            ctx.logger.log('')

            return False
