#!/usr/bin/env python3
"""Run a test command against fresh, funded regtest RPC servers."""

import json
import os
from pathlib import Path
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import time
import uuid


ADDRESS = "4ARcPLqcrRjhZAVm3me1U18rUVXcjeNKuBd1oE2cTs8biA9cozPMeyYLhe77nPv12JA3ejJN3qprmREriit2fi6tJAPbWJu"
MINING_ADDRESS = "42U9v3qs5CjZEePHBZHwuSckQXebuZu299NSmVEmQ41YJZQhKcPyujyMSzpDH4VMMVSBo3U3b54JaNvQLwAjqDhKS3rvM3L"


def main():
    if len(sys.argv) < 2:
        raise SystemExit("Usage: python3 bin/test_regtest.py <test command> [arguments...]")
    bins = Path(os.environ.get("MONERO_BINS_DIR", "external/monero-cpp/external/monero-project/build/release/bin")).resolve()
    daemon_port = int(os.environ.get("MONERO_DAEMON_RPC_PORT", "28081"))
    wallet_port = int(os.environ.get("MONERO_WALLET_RPC_PORT", "28084"))
    for port in (daemon_port, wallet_port):
        with socket.socket() as sock:
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            sock.bind(("127.0.0.1", port))
    work = Path(tempfile.mkdtemp(prefix="monero-regtest-"))
    user, password = "regtest", uuid.uuid4().hex
    processes, logs = [], []
    daemon_ready = False
    result = 1

    def rpc(port, method, params=None, path="/json_rpc"):
        body = {"jsonrpc": "2.0", "id": "0", "method": method, "params": params or {}} if path == "/json_rpc" else params or {}
        response = subprocess.run([
            "curl", "--silent", "--show-error", "--fail", "--digest", "--user", user + ":" + password,
            "--max-time", "60", "--header", "Content-Type: application/json", "--data", json.dumps(body),
            "http://127.0.0.1:" + str(port) + path,
        ], check=True, capture_output=True, text=True)
        response = json.loads(response.stdout)
        if "error" in response:
            raise RuntimeError(response["error"])
        return response.get("result", response)

    def start(name, args, port, method):
        log = (work / (name + ".log")).open("w")
        logs.append(log)
        process = subprocess.Popen([str(bins / name)] + args, stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
        processes.append(process)
        deadline = time.monotonic() + 30
        while time.monotonic() < deadline:
            if process.poll() is not None:
                raise RuntimeError(name + " exited before its RPC server was ready")
            try:
                response = rpc(port, method)
                if process.poll() is not None:
                    raise RuntimeError(name + " exited during startup")
                return response
            except (subprocess.CalledProcessError, OSError):
                time.sleep(0.2)
        raise RuntimeError(name + " RPC startup timed out")

    def stop(process):
        # kill only the process groups created by this runner, including test wallet subprocesses
        try:
            os.killpg(process.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        deadline = time.monotonic() + 5
        while time.monotonic() < deadline:
            process.poll()
            try:
                os.killpg(process.pid, 0)
            except ProcessLookupError:
                return
            time.sleep(0.1)
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        process.wait()

    def interrupted(signum, frame):
        raise KeyboardInterrupt

    signal.signal(signal.SIGTERM, interrupted)
    try:
        for directory in ("daemon", "full", "rpc"):
            (work / directory).mkdir()
        info = start("monerod", [
            "--regtest", "--offline", "--fixed-difficulty", "1", "--non-interactive", "--no-zmq",
            "--p2p-bind-ip", "127.0.0.1", "--p2p-bind-port", str(daemon_port - 1),
            "--rpc-bind-ip", "127.0.0.1", "--rpc-bind-port", str(daemon_port),
            "--rpc-login", user + ":" + password, "--rpc-ssl", "disabled",
            "--data-dir", str(work / "daemon"), "--log-file", str(work / "daemon-detail.log"),
            "--check-updates", "disabled", "--disable-dns-checkpoints",
        ], daemon_port, "get_info")
        if info["nettype"] != "fakechain" or info["height"] != 1:
            raise RuntimeError("Expected a fresh regtest daemon")
        daemon_ready = True
        # preserve the fixture's first receive height (171) and mature every funded output
        for address, count in ((MINING_ADDRESS, 170), (ADDRESS, 100), (MINING_ADDRESS, 60)):
            rpc(daemon_port, "generateblocks", {"wallet_address": address, "amount_of_blocks": count})
        start("monero-wallet-rpc", [
            "--rpc-bind-ip", "127.0.0.1", "--rpc-bind-port", str(wallet_port),
            "--rpc-login", user + ":" + password, "--rpc-ssl", "disabled", "--daemon-ssl", "disabled",
            "--daemon-address", "127.0.0.1:" + str(daemon_port), "--daemon-login", user + ":" + password,
            "--wallet-dir", str(work / "rpc"), "--log-file", str(work / "wallet-detail.log"),
            "--allow-mismatched-daemon-version", "--trusted-daemon", "--non-interactive",
        ], wallet_port, "get_version")
        env = dict(os.environ, MONERO_BINS_DIR=str(bins), MONERO_TEST_WALLETS_DIR=str(work / "full"),
                   MONERO_WALLET_RPC_DIR=str(work / "rpc"), MONERO_DAEMON_RPC_PORT=str(daemon_port),
                   MONERO_WALLET_RPC_PORT=str(wallet_port), MONERO_DAEMON_RPC_USERNAME=user,
                   MONERO_DAEMON_RPC_PASSWORD=password, MONERO_WALLET_RPC_USERNAME=user,
                   MONERO_WALLET_RPC_PASSWORD=password)
        print("Running tests on fresh regtest at height 331", flush=True)
        command = subprocess.Popen(sys.argv[1:], env=env, start_new_session=True)
        processes.append(command)
        result = command.wait()
    except KeyboardInterrupt:
        result = 130
    finally:
        signal.signal(signal.SIGTERM, signal.SIG_IGN)
        if len(processes) > 2:
            stop(processes.pop())
        if daemon_ready:
            try:
                rpc(daemon_port, "stop_daemon", path="/stop_daemon")
                processes[0].wait(timeout=5)
            except (subprocess.SubprocessError, OSError, RuntimeError):
                pass
        for process in reversed(processes):
            stop(process)
        for log in logs:
            log.close()
        if result == 0:
            shutil.rmtree(work)
        else:
            print("Regtest logs and wallets retained at " + str(work), file=sys.stderr)
    return result


if __name__ == "__main__":
    sys.exit(main())
