#!/usr/bin/env python3
#
# List IAM users based on tags

from argparse import ArgumentParser
import csv
import yaml

import boto3


def get_args():
    """Parse CLI arguments"""
    parser = ArgumentParser(
        prog='aws-users',
        description='List IAM users filtered by tags'
    )
    parser.add_argument('-p', '--profile', help='AWS CLI profile to use')
    parser.add_argument('-t', '--tag', help='Tag key name to filter on')
    parser.add_argument(
        'accesshub_csv',
        metavar='FILE_PATH',
        nargs=1,
        help='File path of downloaded AccessHub export csv.'
    )
    return parser.parse_known_args()


def parse_csv(file_name):
    """Parse CSV file into a list of objects"""
    contents = []
    headers = None

    with open(file_name, newline='') as csvfile:
        for row in csv.reader(csvfile, delimiter=',', quotechar='"'):
            # handle first row to store headers list
            if not headers:
                headers = list(row)
            else:
                new_item = {}

                for index, header in enumerate(headers):
                    new_item[header] = row[index]

                contents.append(new_item)

    return contents


def csv_user_roles_map(csv_contents):
    """Build user-roles map from CSV export contents"""
    users = {}

    for obj in csv_contents:
        username = obj['ACCOUNTNAME'].lower()

        if username not in users.keys():
            users[username] = []

        if obj['ENTITLEMENT TYPE'] == 'BASE ACCOUNT':
            users[username].append('readonly')
        elif obj['ENTITLEMENT TYPE'] == 'Group':
            users[username].append(obj['ENTITLEMENT VALUE'].lower())

    return users


def aws_user_roles_map(iam):
    """Generate user-roles map based on AWS current state"""
    users = {}

    for response in iam.get_paginator('list_users').paginate():
        for user in response['Users']:
            if '@' in user['UserName']:
                users[user['UserName']] = list(map(
                    lambda x: x['Key'],
                    iam.list_user_tags(UserName=user['UserName'])['Tags']
                ))

    return users


def main():
    """Main entrypoint"""
    args, _ = get_args()

    if args.profile:
        session = boto3.Session(profile_name=args.profile)
    else:
        session = boto3.Session()

    iam_users = aws_user_roles_map(session.client('iam'))

    csv_users = csv_user_roles_map(parse_csv(args.accesshub_csv[0]))

    # compare for users to delete
    del_users = [
        user
        for user in iam_users.keys()
        if user not in csv_users.keys()
    ]

    if len(del_users):
        print('Users to delete from AWS:')
        print(yaml.dump(del_users, default_flow_style=False))

    # compare for users to add
    add_users = {
        user: csv_users[user]
        for user in csv_users.keys()
        if user not in iam_users.keys()
    }

    if len(add_users):
        print('Users to create in AWS:')
        print(yaml.dump(add_users, default_flow_style=False))

    # compare role differences for existing users
    change_users = {}

    for username in set(csv_users.keys()) & set(iam_users.keys()):
        del_roles = [
            role
            for role in iam_users[username]
            if role not in csv_users[username]
        ]
        add_roles = [
            role
            for role in csv_users[username]
            if role not in iam_users[username]
        ]

        if len(del_roles) or len(add_roles):
            change_users[username] = {}

            if len(del_roles):
                change_users[username]['roles_to_delete'] = del_roles

            if len(add_roles):
                change_users[username]['roles_to_add'] = add_roles

    if len(change_users.keys()):
        print('Users to update in AWS:')
        print(yaml.dump(change_users, default_flow_style=False))


if __name__ == '__main__':
    main()
