import { Edit, useForm, useSelect } from "@refinedev/antd";
import { Button, Form, Input, Select, Space } from "antd";
import { Attributes } from "../../components/view/attributes";
import { Title, groupLevel } from "../../components/view/consts";
import { IStep } from "../../interfaces";
import { MinusCircleOutlined, PlusOutlined } from "@ant-design/icons";
import {
  stepIdsCol,
  result2Flow,
  values2Request,
} from "../../components/domain/flow";

export const FlowEdit = () => {
  const {
    formProps,
    saveButtonProps,
    onFinish,
    queryResult: flowResult,
  } = useForm({});

  const { selectProps } = useSelect<IStep>({
    resource: "steps",
    optionLabel: ((item: any) => `${item.name} - ${item.step_id}`) as any,
    optionValue: "step_id" as any,
  });

  const handleOnFinish = (values: any) => {
    console.log("Success:", values);
    onFinish(values2Request(values));
  };

  const flow = result2Flow(flowResult?.data?.data);

  return (
    <Edit saveButtonProps={saveButtonProps}>
      <Form {...formProps} onFinish={handleOnFinish} layout="vertical">
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
                    initialValue={flow?.stepIds?.[key] as any}
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
                  Add function
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
    </Edit>
  );
};
