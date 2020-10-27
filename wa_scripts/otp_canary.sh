#!/usr/bin/env bash
set -euxo pipefail

erlang_app=${erlang_app:-"void"}
tw_spec=${tw_spec:-~/fbsource/fbcode/tupperware/config/whatsapp/platform.tw}
tw_job=${tw_job:-"tsp_global/whatsapp/sandbox"}
tw_task_id=${tw_task_id:-""}
canary_duration=${canary_duration:-"30m"}
wa_erlang_version=${wa_erlang_version:-""}
wa_multi_erlang_version=${wa_multi_erlang_version:-""}

if [[ ! -z "${wa_multi_erlang_version:=}" && -z "${wa_erlang_version:=}" ]]; then
    echo "wa_multi_erlang_version option also requires specifying wa_erlang_version"
    exit 1
fi

# build new OTP package or use provided one
if [[ -z "${wa_erlang_version:=}" ]]; then
    wa_erlang=$(PATH=.:${PATH} fbpkg build -E wa.erlang.platform009 --not-for-production)
    wa_erlang_version=$(echo ${wa_erlang} | sed 's/.*://')
fi

# build new multi_erlang package or use provided one
if [[ -z "${wa_multi_erlang_version:=}" ]]; then
    wa_multi_erlang=$(ADDITIONAL_ERLANG_VERSION=${wa_erlang_version} build_multi_erlang.sh)
    wa_multi_erlang_version=$(echo ${wa_multi_erlang} | sed 's/.*://')
fi

# fetch multi_erlang package into temp dir
export ME_TEMP=$(mktemp -d)
pushd ${ME_TEMP}
fbpkg.fetch wa.multi_erlang.platform009:${wa_multi_erlang_version}
popd

# build Erlang app with new multi_erlang package
pushd ${ECTL_HOME}
erlang_app_build_json=$(MULTI_ERLANG_PATH=${ME_TEMP} wa-contbuild build ${erlang_app})
app_package_id=$(echo ${erlang_app_build_json} | jq -r '.fbpkg_to_version[]')
erlang_app_package="wa.erl."${erlang_app}":"${app_package_id}
popd

effective_task_id=""
if [[ ! -z "${tw_task_id:=}" ]]; then
    effective_task_id="--tasks="${tw_task_id}
fi

OTP_VERSION_OVERRIDE=${wa_erlang_version} TW_PUSHED_VERSION=${erlang_app_package},wa.multi_erlang.platform009:${wa_multi_erlang_version} tw canary start --all-fields --from-local-spec ${tw_spec} --filter-regex ${tw_job} --duration ${canary_duration} ${effective_task_id}
