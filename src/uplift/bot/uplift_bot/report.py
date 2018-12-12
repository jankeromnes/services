# -*- coding: utf-8 -*-
import itertools
import operator

from cli_common.log import get_logger
from cli_common.taskcluster import get_service

logger = get_logger(__name__)


class Report(object):
    '''
    Build a report during bot execution
    and send it at the end through TC emails
    '''

    def __init__(self, emails):
        self.emails = emails
        self.merges = set()
        logger.info('Report notifications', emails=self.emails)

    def add_invalid_merge(self, merge_test):
        '''
        Mark a bug with an invalid merged test
        '''
        self.merges.add(merge_test)

    def send(self, app_channel, pushed_tests=[]):
        '''
        Build and send report using Taskcluster notification service
        '''
        assert isinstance(pushed_tests, list)

        # Skip sending when there are no failed merges
        if not self.merges and not pushed_tests:
            logger.info('Nothing to report.')
            return

        def _str(x):
            return isinstance(x, bytes) and x.decode('utf-8') or x

        def _commit_report(revision, result):
            fail_fmt = ' * Merge failed for commit `{}` (parent `{}`)\n\n```\n{}```'  # noqa
            default_fmt = ' * Merge {} for commit `{}`'
            if result.status.value == 'failed':
                return fail_fmt.format(
                    _str(revision),
                    _str(result.parent),
                    result.message,
                )

            else:
                return default_fmt.format(result.status, _str(revision))

        # Build markdown output
        # List pushed branches
        mail = [
            '# Pushed branches',
            ''
        ]
        for test in pushed_tests:
            mail.append('* {}'.format(test.branch_rebased))

        # Sorting failed merge tests by bugzilla id & branch
        subject = '[{}] Uplift bot detected {} push & {} merge failures'.format(
            app_channel,
            len(pushed_tests),
            len(self.merges),
        )
        mail += [
            '',
            '# Failed automated merge test',
            ''
        ]
        branches = []
        cmp_func = operator.attrgetter('bugzilla_id', 'branch')
        merges = sorted(self.merges, key=cmp_func)
        merges = itertools.groupby(merges, key=cmp_func)
        for keys, failures in merges:
            bz_id, branch = keys
            branches.append(_str(branch))
            mail.append('## Bug [{0}](https://bugzil.la/{0}) - Uplift to {1}\n'.format(bz_id, _str(branch)))  # noqa
            for merge_test in failures:
                mail += [
                    _commit_report(revision, result)
                    for revision, result in merge_test.results.items()
                ]
            mail.append('')  # newline
        if len(branches) > 0:
            subject += ' to ' + ', '.join(branches)
        mail_md = '\n'.join(mail)

        # Send mail report to every mail address
        notify = get_service('notify')
        for email in self.emails:
            notify.email({
                'address': email,
                'subject': subject,
                'content': mail_md,
                'template': 'fullscreen',
            })
            logger.info('Sent report', to=email)
