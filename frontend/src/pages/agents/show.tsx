import {
  DateField,
  MarkdownField,
  NumberField,
  Show,
  TextField,
} from "@refinedev/antd";
import { useOne, useSelect, useShow } from "@refinedev/core";
import { Typography } from "antd";
import { IFunction } from "../../interfaces";
import { result2Agent } from "../../components/domain/agent";
import { Attributes } from "../../components/view/showAttributes";

const { Title } = Typography;

export const AgentShow = () => {
  const { queryResult } = useShow({});
  const { data, isLoading } = queryResult;

  const { options } = useSelect<IFunction>({
    resource: "functions",
    optionLabel: ((item: any) => `${item.name} - ${item.function_id}`) as any,
    optionValue: "function_id" as any,
  });

  const optionMap = new Map(options?.map((x) => [x?.value, x?.label]));

  const record = data?.data;
  const agent = result2Agent(record);

  return (
    <Show isLoading={isLoading}>
      <Title level={5}>{"ID"}</Title>
      <TextField value={record?.agent_id ?? ""} />
      <Title level={5}>{"Name"}</Title>
      <TextField value={record?.name} />
      <Title level={5}>{"Description"}</Title>
      <MarkdownField value={record?.description} />
      <Title level={5}>{"Version"}</Title>
      <MarkdownField value={record?.version} />
      <Title level={5}>{"Profile"}</Title>
      <TextField value={record?.profile_id} />
      <Title level={5}>{"Flow"}</Title>
      <TextField value={record?.flow_id} />
      <Title level={5}>{"Functions"}</Title>
      {agent.functionIds?.map((fid) => (
        <p>
          <TextField value={optionMap.get(fid) as any} />
        </p>
      ))}
      <Attributes value={record?.attributes} />
    </Show>
  );
};
