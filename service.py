#!/usr/bin/env python3

import os
import io
import sys
import shutil
import subprocess
import time

import click
import ruamel.yaml as ryaml
import jinja2

HERE = os.path.abspath(os.path.dirname(__file__))


def load_settings(service, config):
    service_configs_filepath = os.path.join(HERE, 'services', '{}.yaml'.format(service))
    yaml = ryaml.YAML()
    try:
        with io.open(service_configs_filepath, encoding='utf-8') as f:
            descriptor = yaml.load(f)
    except Exception as e:
        return None, 'Failed to load descriptor for service [{}]: {}'.format(service, e)
    else:
        if not descriptor or 'configs' not in descriptor:
            return None, 'Service descriptor invalid or empty'
        if config not in descriptor['configs']:
            return None, 'Config definition not found: {}'.format(config)
        settings = descriptor.get('common', {})
        settings = {k: v.format(service=service, config=config) for k, v in settings.items()}
        settings.update(descriptor['configs'][config] or {})
        settings['SERVICE'] = service
        settings['CONFIG'] = config
        settings['HOME'] = HERE
        settings['PYTHON_CMD'] = sys.executable
        settings['LOGGING_DIR'] = '/var/log/orderry/qri'
        return settings, None


def render_template(localpath, context):
    loader = jinja2.FileSystemLoader(HERE)
    env = jinja2.Environment(loader=loader)
    env.undefined = jinja2.StrictUndefined
    template = env.get_template(localpath)
    result = template.render(context)
    return result


def derive_systemd_info(service, config):
    service_name = 'orderry.qri.{}.{}'.format(service, config)
    service_path = os.path.join('/etc/systemd/system', '{}.service'.format(service_name))
    return service_name, service_path


@click.group()
def cli():
    """Tool for service control"""
    pass


@cli.command()
@click.argument('service')
@click.argument('config')
def install(service, config):
    """Install systemd service"""
    print('Setting up service [{}] for config [{}]...'.format(service, config))
    settings, error = load_settings(service, config)
    if error is not None:
        print('ERROR:', error, file=sys.stderr)
        sys.exit(1)
    try:
        if service == 'server':
            service_def = render_template('services/templates/qri.service', settings)
            systemd_name, systemd_path = derive_systemd_info(service, config)
            print('Installing to:', systemd_path)
            with io.open(systemd_path, 'w', encoding='utf-8') as ostream:
                shutil.copyfileobj(io.StringIO(service_def), ostream)
            subprocess.run('systemctl daemon-reload'.split(), check=True)
            subprocess.run('systemctl enable {}'.format(systemd_name).split(), check=True)
        else:
            raise Exception('Unsupported service: {}'.format(service))
    except Exception as e:
        print('ERROR: Failed to install as system service:', e, file=sys.stderr)
        sys.exit(1)
    print('Service [{}] configuration [{}] installed'.format(service, config))


@cli.command()
@click.argument('service')
@click.argument('config')
def uninstall(service, config):
    """Uninstall systemd service"""
    print('Removing service [{}] for config [{}]...'.format(service, config))
    settings, error = load_settings(service, config)
    if error is not None:
        print('ERROR:', error, file=sys.stderr)
        sys.exit(1)
    systemd_name, systemd_path = derive_systemd_info(service, config)
    try:
        if os.path.exists(systemd_path):
            subprocess.run('systemctl stop {}'.format(systemd_name).split())
            subprocess.run('systemctl disable {}'.format(systemd_name).split())
            os.remove(systemd_path)
    except FileNotFoundError:
        pass
    except Exception as e:
        print('ERROR: Failed to uninstall system service:', e, file=sys.stderr)
        sys.exit(1)
    else:
        print('Service uninstalled:', systemd_name)


WAIT_FOR_STARTUP = 2.0  # second(s)


@cli.command()
@click.argument('service')
@click.argument('config')
def start(service, config):
    """Start systemd service"""
    print('Starting service [{}] for config [{}]...'.format(service, config))
    systemd_name, systemd_path = derive_systemd_info(service, config)
    p = subprocess.run('systemctl restart {}'.format(systemd_name).split())
    if p.returncode != 0:
        print('ERROR: Failed to start service:', systemd_name, file=sys.stderr)
        sys.exit(1)
    time.sleep(WAIT_FOR_STARTUP)
    p = subprocess.run('systemctl -q status {}'.format(systemd_name).split(), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if p.returncode != 0:
        # TODO: inconsistent behavior: service remains enabled
        subprocess.run('systemctl stop {}'.format(systemd_name).split())
        print('ERROR: Failed to start service:', systemd_name, file=sys.stderr)
        sys.exit(1)
    print('Service started:', systemd_name)


@cli.command()
@click.argument('service')
@click.argument('config')
def stop(service, config):
    """Stop systemd service"""
    print('Stopping service [{}] for config [{}]...'.format(service, config))
    systemd_name, systemd_path = derive_systemd_info(service, config)
    p = subprocess.run('systemctl stop {}'.format(systemd_name).split())
    if p.returncode != 0:
        print('ERROR: Failed to stop service:', systemd_name, file=sys.stderr)
        sys.exit(1)
    else:
        print('Service stopped:', systemd_name)


if __name__ == '__main__':
    cli()
