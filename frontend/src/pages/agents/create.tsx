import { Create, useForm, useSelect } from "@refinedev/antd";
import MDEditor from "@uiw/react-md-editor";
import { Button, Form, Input, Select, Space } from "antd";
import { functionId, functionIdsCol } from "../../components/domain/flow";
import { Attributes } from "../../components/view/attributes";
import { IFlow, IFunction, IProfile } from "../../interfaces";
import { Title, groupLevel } from "../../components/view/consts";
import { MinusCircleOutlined, PlusOutlined } from "@ant-design/icons";
import { values2Request } from "../../components/domain/agent";
import { genUUID } from "../../components/utils";

export const AgentCreate = () => {
  const { formProps, saveButtonProps, onFinish } = useForm({});

  const { selectProps: profileSelectProps } = useSelect<IProfile>({
    resource: "profiles",
    optionLabel: ((item: any) => `${item.name} - ${item.profile_id}`) as any,
    optionValue: "profile_id" as any,
  });

  const { selectProps: flowSelectProps } = useSelect<IFlow>({
    resource: "flows",
    optionLabel: ((item: any) => `${item.name} - ${item.flow_id}`) as any,
    optionValue: "flow_id" as any,
  });

  const { selectProps: functionSelectProps } = useSelect<IFunction>({
    resource: "functions",
    optionLabel: ((item: any) => `${item.name} - ${item.function_id}`) as any,
    optionValue: "function_id" as any,
  });

  const onFinishHandler = (values: any) => {
    onFinish(values2Request(values));
  };

  return (
    <Create saveButtonProps={saveButtonProps}>
      <Form {...formProps} onFinish={onFinishHandler} layout="vertical">
        <Form.Item name={["agent_id"]} hidden={true} initialValue={genUUID()} />
        <Form.Item
          label={"Name"}
          name="name"
          rules={[
            {
              required: true,
            },
          ]}
        >
          <Input />
        </Form.Item>
        <Form.Item label={"Description"} name="description">
          <MDEditor data-color-mode="light" />
        </Form.Item>
        <Form.Item
          label={"Version"}
          name="version"
          rules={[
            {
              required: true,
            },
          ]}
          initialValue={"1.0.0"}
        >
          <Input />
        </Form.Item>

        <Form.Item label={"Profile"} name={["profile_id"]}>
          <Select
            {...profileSelectProps}
            style={{ width: 800, marginBottom: 24 }}
          />
        </Form.Item>

        <Form.Item label={"Flow"} name={["flow_id"]}>
          <Select
            {...flowSelectProps}
            style={{ width: 800, marginBottom: 24 }}
          />
        </Form.Item>

        <Title level={groupLevel}>Functions</Title>
        <Form.List name={[functionIdsCol]}>
          {(fields, { add, remove }) => (
            <>
              {fields.map(({ key, name, ...restField }) => (
                <Space
                  key={key}
                  style={{ display: "flex", marginBottom: 8 }}
                  align="baseline"
                >
                  <Form.Item
                    {...restField}
                    name={[name, functionId]}
                    rules={[{ required: true, message: "Missing function" }]}
                    labelAlign="left"
                  >
                    <Select
                      {...functionSelectProps}
                      style={{ width: 800, marginBottom: 24 }}
                    />
                  </Form.Item>
                  <MinusCircleOutlined onClick={() => remove(name)} />
                </Space>
              ))}
              <Form.Item>
                <Button
                  type="dashed"
                  onClick={() => add()}
                  block
                  icon={<PlusOutlined />}
                >
                  Add function
                </Button>
              </Form.Item>
            </>
          )}
        </Form.List>

        <Attributes />
      </Form>
    </Create>
  );
};
