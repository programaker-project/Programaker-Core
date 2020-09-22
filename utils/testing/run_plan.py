#!/usr/bin/env python3

import copy
import json
import os
import subprocess
import sys
import time
import traceback

import pygraphviz as pgv
import requests
from colorama import Back, Fore, Style

import builder
import gen_test_api_plan as test_api

api_groups = test_api.parse(test_api.read_permission_matrix(
    test_api.DATA_FILE))
test_api.add_api_requirements(api_groups)

ENDPOINTS = {}

for content in api_groups.values():
    for item in content['items']:
        ENDPOINTS[item['url']] = item


def reset_ctx(ctx):
    del ctx['token']


def promote_to_admin(ctx):
    out = subprocess.check_output([
        "docker", "exec", ctx['docker'], "/app/scripts/run_erl.sh",
        "{ok, {user, UserId}} = automate_storage:get_userid_from_username(<<\""
        + ctx['user_name'] + "\">>)," +
        "automate_storage:promote_user_to_admin(UserId)"
    ]).decode('utf-8').strip()

    if out != 'ok':
        raise AssertionError("Expected 'ok', found {}".format(out))


def get_user_id_from_data(ctx):
    headers = {'Authorization': ctx['token']}
    res = requests.post('{}/sessions/login'.format(ctx['root'].strip('/')),
                        json={
                            'password': ctx['password'],
                            'username': ctx['user_name'],
                        },
                        headers=headers)
    data = res.json()
    return (data['user_id'], data['token'])


def negate_execute_op(op, ctx):
    failed = False
    try:
        res = execute_op(op, ctx)
        if res == 'ignored':
            return res
    except:
        failed = True

    if not failed:
        raise AssertionError('Operation expected to fail, but succeeded')


def execute_op(op, ctx):
    if op == 'promote_to_admin':
        promote_to_admin(ctx)
        return
    if op == 'logout':
        reset_ctx(ctx)
        return 0
    if op == 'jump_to_admin':
        ctx['token'] = ctx['_admin_token']
        return 0

    headers = {}
    if 'token' in ctx:
        headers['Authorization'] = ctx['token']

    if op == 'make-editor':
        user_id, new_token = get_user_id_from_data(ctx)
        res = requests.post('{}/groups/by-id/{}/collaborators'.format(
            ctx['root'].strip('/'), ctx['group_id']),
                            json={
                                'action':
                                'invite',
                                'collaborators': [{
                                    'id': user_id,
                                    'role': 'editor'
                                }]
                            },
                            headers=headers)
        res.raise_for_status()
        ctx['user_id'] = user_id
        ctx['token'] = new_token
        return 2
    if op == 'make-viewer':
        user_id, new_token = get_user_id_from_data(ctx)
        res = requests.post('{}/groups/by-id/{}/collaborators'.format(
            ctx['root'].strip('/'), ctx['group_id']),
                            json={
                                'action':
                                'invite',
                                'collaborators': [{
                                    'id': user_id,
                                    'role': 'viewer'
                                }]
                            },
                            headers=headers)
        res.raise_for_status()
        ctx['user_id'] = user_id
        ctx['token'] = new_token
        return 2

    verb, endpoint = op.split()
    path = ctx['root'].strip('/') + '/' + endpoint.strip('/')
    if verb == 'get':
        data = builder.build_data_for_query(verb, endpoint, ctx)
        path = builder.fill_path_params(path, ctx)
        res = requests.get(path, headers=headers, params=data)
        res.raise_for_status()

    elif verb == 'post':
        data = builder.build_data_for_query(verb, endpoint, ctx)
        path = builder.fill_path_params(path, ctx)
        if isinstance(data, tuple):
            res = requests.post(path,
                                json=data[0],
                                files=data[1],
                                headers=headers)
        else:
            res = requests.post(path, json=data, headers=headers)
        res.raise_for_status()
        builder.update_ctx(verb, endpoint, res, ctx)

    elif verb == 'delete':  # TODO Will be required to happen after all ops on resource happened
        return 'ignored'
        # data = builder.build_data_for_query(verb, endpoint, ctx)
        # path = builder.fill_path_params(path, ctx)
        # res = requests.delete(path, json=data, headers=headers)
        # res.raise_for_status()
        # builder.update_ctx(verb, endpoint, res, ctx)

    elif verb == 'patch':  # TODO
        return 'ignored'

    elif verb == 'put':  # TODO
        return 'ignored'

    elif verb == 'write':  # WS verb
        return 'ignored'

    elif verb == 'read':  # WS verb
        return 'ignored'

    elif verb in ('check_noauth', 'mayget'):
        if verb == 'check_noauth':
            tested_verbs = 'get', 'post', 'delete', 'put', 'patch'
        elif verb == 'mayget':
            tested_verbs = 'post', 'delete', 'put', 'patch'  # No GET tested

        test_ctx = copy.deepcopy(ctx)
        for test_verb in tested_verbs:
            data = builder.build_data_for_query(test_verb, endpoint, test_ctx)
            path = builder.fill_path_params(path, test_ctx)
            if test_verb == 'get':
                res = requests.get(path, headers=headers, params=data)
            elif isinstance(data, tuple):
                res = requests.__dict__[test_verb](path,
                                                   json=data[0],
                                                   files=data[1],
                                                   headers=headers)
            else:
                res = requests.__dict__[test_verb](path,
                                                   json=data,
                                                   headers=headers)

            if res.ok:
                raise Exception(
                    "Auth passed (incorrectly). Verb: {}".format(test_verb))
        return len(tested_verbs)
    else:
        raise Exception('Unknown verb: ' + verb)


def gen_admin_user(ctx):
    ctx = copy.deepcopy(ctx)
    execute_op('POST /sessions/register'.lower(), ctx)
    execute_op('POST /sessions/login'.lower(), ctx)
    execute_op('promote_to_admin', ctx)
    return (
        ctx['token'],
        ctx['user_id'],
        ctx['user_name'],
    )


def main(test_plan):
    ctx = {
        'root': os.getenv('API_TEST_ROOT', 'http://localhost:8881/api/v0'),
        'docker': os.getenv('API_TEST_DOCKER', 'back-test-docker'),
    }

    plan = pgv.AGraph(test_plan, strict=False, directed=True)
    results = pgv.AGraph(strict=False, directed=True)
    results.graph_attr['rankdir'] = 'LR'
    results.node_attr['shape'] = 'rect'

    edge_idx = {}
    for (_from, to) in plan.edges():
        if not _from in edge_idx:
            edge_idx[_from] = []
        edge_idx[_from].append(to)

    skipped_nodes = set(plan.nodes())

    rem_nodes = [('start', 'start', ctx)]
    skipped_nodes.remove('start')

    endpoints_failed = []

    times = []
    ignored = []
    test_start = time.time()

    admin = gen_admin_user(ctx)
    ctx['_admin_token'], ctx['_admin_id'], ctx['_admin_name'] = admin
    run_node_count = 0
    succeeded_count = 0

    while len(rem_nodes) > 0:
        (node, node_id, ctx) = rem_nodes.pop()

        # Don't allow one branch context to affect another branch.
        #
        # Note that this context will be passed by reference to the next nodes
        # and after that it will be modified in place by the current node's
        # operation.
        ctx = copy.deepcopy(ctx)

        lines = plan.get_node(node).attr['label'].split('\n')
        op = lines[0]
        tags = {}
        label = op

        if len(lines) > 1:
            tags = json.loads('\n'.join(lines[1:]).strip())

        if 'section' in tags:
            label = '{} [{}]'.format(op, tags['section'])

        try:
            t0 = time.time()
            tags['auth'] = bool(ctx.get('token', None))
            if op.upper() != 'START':
                run_node_count += 1
                print("\r\x1b[K … {} {} ".format(op, tags), flush=True, end='')

                if op.lower().startswith('fail-'):
                    result = negate_execute_op(op[5:].lower(), ctx)
                else:
                    result = execute_op(op.lower(), ctx)

                test_time = time.time() - t0
                if result is None or isinstance(result, int):
                    succeeded_count += 1
                    if isinstance(result, int):
                        for i in range(result):
                            times.append((op, test_time / result))
                    else:
                        times.append((op, test_time))

                    print("\r{} ✓ {}  [{:.03f}s] {}".format(
                        Fore.GREEN, op, test_time, tags),
                          Style.RESET_ALL,
                          end='')
                    results.add_node(node_id,
                                     label=label,
                                     style='filled',
                                     fillcolor='green',
                                     color='black',
                                     fontsize=14,
                                     fontcolor='white')
                elif result == 'ignored':
                    ignored.append(op)
                    print(
                        "\r{} I {}  [{:.03f}s] {}".format(
                            Fore.BLUE, op, test_time, tags, node),
                        Style.RESET_ALL)
                    results.add_node(node_id,
                                     label=label,
                                     style='filled',
                                     fillcolor='lightblue',
                                     color='black',
                                     fontsize=14,
                                     fontcolor='black')
            else:
                results.add_node(node_id, shape='doublecircle')
        except Exception as ex:
            print(
                "\r{} × {}  [{:.03f}s] {}".format(Fore.RED, op,
                                                  time.time() - t0, tags),
                Style.RESET_ALL)
            print(traceback.format_exc())
            results.add_node(node_id,
                             label=label,
                             style='filled',
                             shape='signature',
                             fillcolor='red',
                             color='black',
                             fontsize=16,
                             fontcolor='white')
            results.draw('results.svg', prog='dot')
            endpoints_failed.append(op)
            continue

        for tgt in edge_idx.get(node, []):
            tgt_id = test_api.gen_id()

            if node_id is not None:
                results.add_edge(node_id, tgt_id)

            rem_nodes.append((tgt, tgt_id, ctx))
            skipped_nodes.discard(tgt)

    num_nodes = len(plan.nodes()) - 1  # For the "start" node
    num_calls = len(times)
    test_time = time.time() - test_start

    print("\r\x1b[K", end="")
    print("{} nodes NOT tested ({:.2%})".format(len(skipped_nodes),
                                                len(skipped_nodes) /
                                                num_nodes))
    print("{} paths FAILED ({:.2%})".format(len(endpoints_failed),
                                            len(endpoints_failed) / num_nodes))
    print("{} nodes IGNORED ({:.2%})".format(len(ignored),
                                             len(ignored) / num_nodes))
    print("{} nodes SUCCEEDED ({:.2%})".format(succeeded_count,
                                               succeeded_count / num_nodes))

    print(
        "{} calls from {} nodes completed in {:.02f} seconds. (Avg: {:.03f}s)".
        format(num_calls, run_node_count - len(ignored), test_time,
               test_time / num_calls))

    print("10 Slowest operations:")
    deduped = {}
    for (test, duration) in times:
        if test not in deduped:
            deduped[test] = duration
        elif deduped[test] < duration:
            deduped[test] = duration

    for (test, duration) in sorted(deduped.items(),
                                   key=lambda x: x[1],
                                   reverse=True)[:10]:
        print(' {:.03f}s: {}'.format(duration, test))

    results.draw('results.svg', prog='dot')
    if len(endpoints_failed) == 0:
        return 0
    else:
        return 1


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("{} <test-plan.gv>".format(sys.argv[0]))
        exit(2)
    exit(main(sys.argv[1]))
