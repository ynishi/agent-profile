import { Create, useForm, useSelect } from "@refinedev/antd";
import { Button, Form, Input, Select, Space } from "antd";
import { stepIdsCol, values2Request } from "../../components/domain/flow";
import { genUUID } from "../../components/utils";
import { Attributes } from "../../components/view/attributes";
import { IStep } from "../../interfaces";
import { Title, groupLevel } from "../../components/view/consts";
import { MinusCircleOutlined, PlusOutlined } from "@ant-design/icons";

export const FlowCreate = () => {
  const { formProps, saveButtonProps, onFinish } = useForm({});

  const { selectProps } = useSelect<IStep>({
    resource: "steps",
    optionLabel: ((item: any) => `${item.name} - ${item.step_id}`) as any,
    optionValue: "step_id" as any,
  });

  const onFinishHandler = (values: any) => {
    console.log("Success:", values);
    onFinish(values2Request(values));
  };

  return (
    <Create saveButtonProps={saveButtonProps}>
      <Form {...formProps} onFinish={onFinishHandler} layout="vertical">
        <Form.Item
          label={"FlowId"}
          name={["flow_id"]}
          hidden={true}
          initialValue={genUUID()}
        />
        <Form.Item
          label={"Name"}
          name={["name"]}
          rules={[
            {
              required: true,
            },
          ]}
        >
          <Input />
        </Form.Item>
        <Form.Item label={"Description"} name={["description"]}>
          <Input />
        </Form.Item>
        <Title level={groupLevel}>Steps</Title>
        <Form.List name={[stepIdsCol]}>
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
                    name={[name, "stepId"]}
                    rules={[{ required: true, message: "Missing step" }]}
                    labelAlign="left"
                  >
                    <Select
                      {...selectProps}
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
                  Add step
                </Button>
              </Form.Item>
            </>
          )}
        </Form.List>

        <Attributes />
        <Form.Item label={"Error Policy"} name={["error_policy"]}>
          <Input />
        </Form.Item>
      </Form>
    </Create>
  );
};
