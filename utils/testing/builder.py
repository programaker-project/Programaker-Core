import os
import re
import uuid


def gen_rand():
    return str(uuid.uuid4()).replace('-', '')


_up = os.path.dirname

SAMPLE_PICTURE = os.path.join(_up(_up(_up(os.path.abspath(__file__)))),
                              'frontend', 'src', 'assets', 'about-logo.png')


def build_data_for_query(verb, endpoint, ctx):
    if verb.lower() == 'get':
        if endpoint == '/utils/autocomplete/users':
            return {'q': ctx.get('user_name', 'sample')}
        else:
            return {}

    if endpoint == '/sessions/register':
        id = 'test' + gen_rand()
        passwd = 'pass' + gen_rand()
        ctx['user_name'] = id
        ctx['password'] = passwd

        return {
            'email': id + '@test' + id + '.com',
            'password': passwd,
            'username': id,
        }

    elif endpoint == '/sessions/login':
        return {
            'password': ctx['password'],
            'username': ctx['user_name'],
        }

    elif endpoint == '/users/:user_name/monitors' or endpoint == '/programs/by-id/:program_id/monitors':
        id = gen_rand()
        return {
            "type": "http",
            "value": id,
            "name": id,
        }

    elif endpoint in ('/users/id/:user_id/programs/id/:program_id/tags',
                      '/programs/by-id/:program_id/tags'):
        tags = [gen_rand() for _ in range(4)]
        ctx['program_tags'] = tags
        return {
            'tags': tags,
        }

    elif endpoint == '/groups':
        group_name = 'group' + gen_rand()
        ctx['group_name'] = group_name
        return {
            "name": group_name,
            "public": False,
        }

    elif endpoint in ('/users/:user_name/bridges',
                      '/users/id/:user_id/bridges'):
        bridge_name = 'bridge' + gen_rand()
        ctx['bridge_name'] = bridge_name
        return {
            "name": bridge_name,
            "public": False,
        }

    elif endpoint == '/users/id/:user_id/templates':
        template_name = 'template' + gen_rand()
        ctx['template_name'] = template_name
        content = 'this is a template test'
        return {
            "name": template_name,
            "content": content,
        }

    elif endpoint == '/users/id/:user_id/custom_signals':
        signal_name = 'signal' + gen_rand()
        ctx['signal_name'] = signal_name
        return {
            "name": signal_name,
        }

    elif endpoint == '/groups/by-id/:group_id/picture':
        return ({}, {
            'file': open(SAMPLE_PICTURE, 'rb'),
        })

    elif endpoint in ('/users/id/:user_id/programs/id/:program_id/status',
                      '/programs/by-id/:program_id/status'):
        return {
            'enable': True,
        }

    elif endpoint == '/groups/by-id/:group_id/bridges':
        bridge_name = 'bridge' + gen_rand()
        ctx['bridge_name'] = bridge_name
        return {
            "name": bridge_name,
        }

    else:
        return {}


def update_ctx(verb, endpoint, res, ctx):
    if verb == 'post' and endpoint == '/sessions/login':
        data = res.json()
        ctx['token'] = data['token']
        ctx['user_id'] = data['user_id']

    elif verb == 'post' and endpoint == '/groups':
        data = res.json()
        ctx['group_id'] = data['group']['id']

    elif verb == 'post' and endpoint == '/groups/by-id/:group_id/programs':
        data = res.json()
        ctx['program_id'] = data['id']

    elif verb == 'post' and endpoint == '/users/:user_name/programs':
        data = res.json()
        ctx['program_id'] = data['id']

    elif verb == 'post' and endpoint in ('/users/:user_name/bridges',
                                         '/users/id/:user_id/bridges'):
        data = res.json()
        bridge_id = data['control_url'].rstrip('/').split('/')[-2]
        ctx['bridge_id'] = bridge_id

    elif verb == 'post' and endpoint == '/users/id/:user_id/templates':
        data = res.json()
        ctx['template_id'] = data['id']

    elif verb == 'post' and endpoint == '/users/id/:user_id/custom_signals':
        data = res.json()
        ctx['signal_id'] = data['id']


def fill_path_params(endpoint, ctx):
    def _fill_param(chunk):
        return ctx[chunk.group(1)]

    return re.sub(r':([a-zA-Z_]+)', _fill_param, endpoint)
