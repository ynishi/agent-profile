import { NumberField, Show, TextField } from "@refinedev/antd";
import { useShow } from "@refinedev/core";
import { Typography } from "antd";

import { result2Function } from "../../components/domain/function";
import { Attributes } from "../../components/view/showAttributes";

const { Title } = Typography;

export const FunctionShow = () => {
  const { queryResult } = useShow({});
  const { data, isLoading } = queryResult;

  const record = data?.data;
  const functionData = result2Function(record);

  return (
    <Show isLoading={isLoading}>
      <Title level={5}>{"FunctionId"}</Title>
      <TextField value={functionData.functionId} />
      <Title level={5}>{"Name"}</Title>
      <TextField value={functionData.name} />
      <Title level={5}>{"Description"}</Title>
      <TextField value={functionData.description} />
      <Title level={5}>{"Content"}</Title>
      <TextField value={functionData.content} />
      <Title level={5}>{"InputData"}</Title>
      <TextField value={functionData.inputData} />
      <Title level={5}>{"OutputData"}</Title>
      <TextField value={functionData.outputData} />
      <Attributes value={functionData.attributes} />
    </Show>
  );
};
