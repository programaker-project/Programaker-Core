#!/usr/bin/env python3

import csv
import itertools
import json
import logging
import os
import re
import uuid

import pygraphviz as pgv

DATA_FILE = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))), 'docs', 'test-table.csv')

GET_URL_PARAM_RE = re.compile(':([a-zA-Z_]+)')

if not os.path.exists(DATA_FILE):
    raise Exception("Test table not found (expected at {})".format(DATA_FILE))


def gen_id():
    return str(uuid.uuid4())


def read_permission_matrix(fname):
    return csv.reader(open(DATA_FILE, 'rt'))


def parse(rows):
    groups = {}
    current_group = None
    sections = []
    for row in rows:
        if len(row) == 0 or len(row[0]) == 0:
            continue

        if row[0].startswith('/'):
            if current_group is None:
                raise Exception("No group defined before " + row[0])

            if '[skip]' in row[0].lower():
                continue

            req = ''
            prod = ''
            if len(row) > 1:
                req = row[1]
            if len(row) > 2:
                prod = row[2]

            element = {
                'raw':
                row,
                'url':
                row[0],
                'requires': [
                    part.lower().strip(', ') for part in req.split(' ')
                    if part != ''
                ],
                'produces': [
                    part.lower().strip(', ') for part in prod.split(' ')
                    if part != ''
                ],
                'sections': {},
                'id':
                gen_id(),
            }

            for off, section in enumerate(sections):
                if len(section) == 0:
                    continue
                idx = off + 3
                if len(row) > idx:
                    element['sections'][section.upper()] = [
                        part.strip(', ') for part in row[idx].split(' ')
                        if part != ''
                    ]
                else:
                    element['sections'][section.upper()] = []

            groups[current_group]['items'].append(element)
        else:
            if current_group:  # Remove group if no elements were added
                if len(groups[current_group]['items']) == 0:
                    del groups[current_group]

            current_group = row[0].lower()
            groups[current_group] = {'sections': row[3:], 'items': []}
            sections = row[3:]
    return groups


def add_api_requirements(groups):
    for (group, contents) in groups.items():
        for endpoint in contents['items']:
            url = endpoint['url']
            requirements = []
            params = GET_URL_PARAM_RE.findall(url)
            for param in params:
                field = None
                if '_' in param:
                    name, field = param.split('_')
                else:
                    name = param

                requirements.append((name, field))
            endpoint['requires'] = merge_requirements(
                requirements, [(req, None) for req in endpoint['requires']])


def merge_requirements(g1, g2):
    requirements = {}
    for (req, prop) in g1:
        if req not in requirements:
            requirements[req] = prop
        elif requirements[req] == prop:
            requirements[req] = None
    for (req, prop) in g2:
        if req not in requirements:
            requirements[req] = prop
        elif requirements[req] == prop:
            requirements[req] = None
    return list(requirements.items())


def complement_permissions(perms):
    verbs = ('GET', 'POST', 'PUT', 'PATCH', 'DELETE')
    if any([verb in perms for verb in verbs]):
        # If any of the known permissions is give, negate the others
        for verb in ('GET', 'POST', 'PUT', 'PATCH', 'DELETE'):
            if verb not in perms:
                perms.append("FAIL-" + verb)


def build_test_plan(groups):
    plan = pgv.AGraph(directed=True,
                      comment='PrograMaker API test dependencies')
    plan.graph_attr['rankdir'] = 'LR'
    plan.node_attr['shape'] = 'rect'

    start_id = gen_id()
    plan.add_node('start', label='start')
    plan.add_node('logout', label='logout')

    plan.add_node('register-admin', label='POST /sessions/register')
    plan.add_node('login-admin', label='POST /sessions/login')
    plan.add_node('promote_to_admin', label='promote_to_admin')

    plan.add_edge('start', 'register-admin')
    plan.add_edge('register-admin', 'login-admin')
    plan.add_edge('login-admin', 'promote_to_admin')

    plan.add_node('register-editor', label='POST /sessions/register')
    plan.add_node('make-editor', label='make-editor')

    plan.add_edge('register-editor', 'make-editor')

    plan.add_node('register-viewer', label='POST /sessions/register')
    plan.add_node('make-viewer', label='make-viewer')

    plan.add_edge('register-viewer', 'make-viewer')

    has_known_productors = set()
    productor_dependencies = {}
    known_requirements = set()

    expanded_blocks = []
    for (group, contents) in groups.items():
        for endpoint in contents['items']:
            for (section, permissions) in endpoint['sections'].items():

                if permissions == ['IGNORE']:
                    # This is used for login endpoints that can be accessed
                    # by non-anonymous users, so not all the following combinations are explored:
                    #
                    # /session/login -> ENDPOINT_X
                    # /session/login -> /session/login -> ENDPOINT_X
                    #
                    # This also is used to mark ADMIN operations that can be
                    # done by registered user.
                    continue

                req = endpoint['requires'].copy()
                for (dep, prop) in req:
                    known_requirements.add(dep)

                ep_id = gen_id()
                tags = {'section': section.upper()}
                produces = endpoint['produces'].copy()
                if endpoint['url'] != '/sessions/login':
                    if section.upper() != 'ANONYMOUS' and (
                            permissions == []
                            and endpoint['sections']['ANONYMOUS'] != []):
                        continue

                    # if section.upper() == 'ANONYMOUS' or (
                    #         permissions != endpoint['sections']['ANONYMOUS']):

                    if section.upper() == 'ADMIN':
                        req.append(('promote_to_admin', None))

                    elif section.upper() == 'G. ADMIN':
                        req.append(('group', None))

                    elif section.upper() == 'G. EDITOR':
                        req.append(('group-editor', None))

                    elif section.upper() == 'G. VIEWER':
                        req.append(('group-viewer', None))

                    elif section.upper() == 'USER':
                        req.append(('login', None))

                    elif section.upper() == 'ANONYMOUS':
                        if len([(res, prop) for (res, prop) in req]) > 0:
                            req.append(('logout', None))

                    else:
                        raise Exception(
                            "Unknown user type: '{}' on '{}'".format(
                                section, endpoint['url']))
                        continue

                for prod in endpoint['produces']:
                    has_known_productors.add(prod)
                    if prod not in productor_dependencies:
                        productor_dependencies[prod] = set()

                    for (dep, prop) in req:
                        productor_dependencies[prod].add(dep)

                if len(permissions) == 0:
                    permissions = ['CHECK_NOAUTH']
                else:
                    complement_permissions(permissions)

                for permission in permissions:
                    permission_produces = produces

                    if permission != 'POST':
                        permission_produces = []
                    block = {
                        'id': ep_id,
                        'requires': req,
                        'url': permission + ' ' + endpoint['url'],
                        'produces': permission_produces,
                        'tags': tags,
                    }

                    expanded_blocks.append(block)

    sealed = set()
    productors = {
        'promote_to_admin': ['promote_to_admin'],
        'logout': ['logout'],
        'group-editor': ['make-editor'],
        'group-viewer': ['make-viewer'],
    }
    has_known_productors.add('promote_to_admin')
    productor_dependencies['promote_to_admin'] = set(['login'])
    has_known_productors.add('logout')
    productor_dependencies['logout'] = set(['login'])

    has_known_productors.add('group-editor')
    productor_dependencies['group-editor'] = set(['group'])
    has_known_productors.add('group-viewer')
    productor_dependencies['group-viewer'] = set(['group'])

    for dep in known_requirements:
        if dep not in has_known_productors:
            logging.warning("No productor known for: {}".format(dep))

    def is_transitively_produced(produced, requirement):
        if requirement not in has_known_productors:
            return False
        if requirement == produced:
            return True
        for req in productor_dependencies[requirement]:
            if is_transitively_produced(produced, req):
                return True
        return False

    def check_valid_order(l):
        for i in range(0, len(l)):
            for j in range(i + 1, len(l)):
                if is_transitively_produced(l[j], l[i]):
                    print("Expected '{}' before '{}'".format(l[j], l[i]))
                    assert not is_transitively_produced(l[j], l[i])

    def priority_sort(items):
        produced_items = []
        remaining_items = items.copy()
        rounds_skipped = 0
        while remaining_items:
            item = remaining_items.pop(
                0
            )  # Note that we're adding items on one end and removing it on the other
            if all([
                    dep in produced_items
                    for dep in productor_dependencies[item]
            ]):
                produced_items.append(item)
                rounds_skipped = 0
            else:
                remaining_items.append(item)
                rounds_skipped += 1

            if rounds_skipped > len(remaining_items):
                print(
                    json.dumps(
                        {
                            k: list(v)
                            for k, v in productor_dependencies.items()
                        },
                        indent=4))
                raise Exception(
                    'Error finding productor order. Ordered: {}. Remaining: {}.'
                    .format(produced_items, remaining_items))
        return produced_items

    priority_list = priority_sort(list(has_known_productors))

    def priority_sort_blocks(blocks):
        remaining_blocks = blocks.copy()
        allowed_dependencies = set()
        for p in [None] + priority_list:
            if p is not None:
                allowed_dependencies.add(p)

            skipped_blocks = []
            for block in remaining_blocks:
                if all([
                        dep in allowed_dependencies
                        for (dep, prop) in block['requires']
                ]):
                    yield block
                else:
                    skipped_blocks.append(block)
            remaining_blocks = skipped_blocks

        if len(remaining_blocks) > 0:
            logging.warning('{} blocks left outside of priority list'.format(
                len(remaining_blocks)))
            for block in remaining_blocks:
                yield block

    order = list(priority_sort_blocks(expanded_blocks))
    for block_num, block in enumerate(order):
        requirement_combos = []
        if len(block['requires']) == 0:
            requirement_combos.append(('start', ))

        doable = True
        for (dep, prop) in block['requires']:
            prods = productors.get(dep, [])
            sealed.add(dep)
            if len(prods) == 0:
                if dep not in has_known_productors:
                    doable = False
                    # logging.warning("No productors for '{}' on '{}'".format(
                    #     dep, block['url']))
                else:
                    raise Exception(
                        "No productor found for '{}' on '{}'. But productors are known. In block {}/{}"
                        .format(dep, block['url'], block_num, len(order)))
            elif len(prods) == 1:
                requirement_combos.append((prods[0], ))
            else:
                requirement_combos.append([prod for prod in prods])

        combos = list(itertools.product(*requirement_combos))
        if not doable:
            combos = [
                ()
            ]  # Show the block on the graph, but outside the dependency tree

        for prod_combo in combos or [()]:
            block_id = gen_id()
            if block['url'] == 'POST /sessions/login':
                login_id = block_id

            if block['url'] == 'POST /groups':
                group_id = block_id

            for prod in block['produces']:
                if prod in sealed:
                    raise Exception(
                        "'{}' produced *after* a block that required it has been placed. In block {}/{}."
                        .format(prod, block_num, len(order)))

                res = prod.lower().strip()
                if res not in productors:
                    productors[res] = []
                productors[res].append(block_id)

            plan.add_node(
                block_id,
                label=block['url'] +
                '\n\n {}'.format(json.dumps(block['tags'])),
            )
            for source in prod_combo:
                plan.add_edge(source, block_id)

    plan.add_edge(login_id, 'logout')
    plan.add_edge(group_id, 'register-editor')
    plan.add_edge(group_id, 'register-viewer')
    return plan


def build_rev_index(plan):
    edge_rev_index = {}
    for (_from, to) in plan.edges():
        if not to in edge_rev_index:
            edge_rev_index[to] = []
        edge_rev_index[to].append(_from)
    return edge_rev_index


def fix_session_management_graph(plan):
    edge_rev_index = build_rev_index(plan)
    blocks_added = {'logout': {}, 'jump_to_admin': {}}

    # Find blocks that have 2 ancestors and one of them is logout or promote_to_admin, then merge all those branches
    has_errors = False
    for node in plan.nodes():
        if len(edge_rev_index.get(node, [])) < 2:
            continue

        block_type = None
        old_block = None
        if 'logout' in edge_rev_index[node]:
            old_block = block_type = 'logout'
        if 'promote_to_admin' in edge_rev_index[node]:
            old_block = 'promote_to_admin'
            block_type = 'jump_to_admin'

        if block_type is None:
            # Nothing can be done here to fix it. Maybe later
            # `prune_incorrect_dependencies()` will fix it.
            # Otherwise it will throw an exception.
            continue

        other_block = [
            block for block in edge_rev_index[node] if block != old_block
        ][0]
        plan.remove_edge(old_block, node)
        plan.remove_edge(other_block, node)
        if other_block not in blocks_added[block_type]:
            block_id = gen_id()
            blocks_added[block_type][other_block] = block_id
            plan.add_node(block_id, label=block_type)
            plan.add_edge(other_block, block_id)

        plan.add_edge(blocks_added[block_type][other_block], node)


def prune_incorrect_dependencies(plan):
    edge_rev_index = build_rev_index(plan)

    # Build equivalences table
    equivs = {}
    for node in plan.nodes():
        op = plan.get_node(node).attr['label']
        if op not in equivs:
            equivs[op] = []
        equivs[op].append(node)

    edge_rev_index = build_rev_index(plan)

    # Check that blocks have only zero or one ancestor
    has_errors = False
    for node in plan.nodes():
        if len(edge_rev_index.get(node, [])) > 1:
            op = plan.get_node(node).attr['label']
            has_option = False
            for eq in equivs[op]:
                if len(edge_rev_index.get(op, [])) < 2:
                    has_option = True
                    break

            if has_option:
                plan.remove_node(node)
            else:
                logging.error("Node has multiple ancestors: {}".format(op))
                has_errors = True

    assert not has_errors


if __name__ == '__main__':
    api_groups = parse(read_permission_matrix(DATA_FILE))
    add_api_requirements(api_groups)
    test_plan = build_test_plan(api_groups)
    logging.getLogger().setLevel(logging.INFO)

    logging.info('Base plan: {} operations'.format(len(test_plan.nodes())))

    test_plan.tred()
    fix_session_management_graph(test_plan)
    prune_incorrect_dependencies(test_plan)
    test_plan.write('plan.gv')
    test_plan.draw('plan.svg', prog='dot')
    logging.info('Final plan: {} operations'.format(len(test_plan.nodes())))
