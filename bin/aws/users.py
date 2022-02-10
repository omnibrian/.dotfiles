#!/usr/bin/env python3
#
# List IAM users based on tags

from argparse import ArgumentParser

import boto3


def get_args():
    """Parse CLI arguments"""
    parser = ArgumentParser(
        prog='aws-users',
        description='List IAM users filtered by tags'
    )
    parser.add_argument('-p', '--profile', help='AWS CLI profile to use')
    parser.add_argument('-t', '--tag', help='Tag key name to filter on')
    return parser.parse_known_args()


def main():
    """Main entrypoint"""
    args, _ = get_args()

    if args.profile:
        session = boto3.Session(profile_name=args.profile)
    else:
        session = boto3.Session()

    iam = session.client('iam')

    if args.tag:
        print(f'Listing users with tag: {args.tag}')

    for response in iam.get_paginator('list_users').paginate():
        for user in response['Users']:
            if args.tag:
                tags = iam.list_user_tags(UserName=user['UserName'])['Tags']

                if args.tag not in map(lambda x: x['Key'], tags):
                    continue

            print(f'- UserName: {user["UserName"]}')
            print(f'  Arn: {user["Arn"]}')


if __name__ == '__main__':
    main()
