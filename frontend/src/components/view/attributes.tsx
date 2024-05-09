import { MinusCircleOutlined, PlusOutlined } from "@ant-design/icons";
import { Button, Form, Input, Space } from "antd";
import { groupLevel, subGroupLevel, Title } from "./consts";

export const Attributes = ({ wrapper }: any) => (
  <>
    <Title level={wrapper ? subGroupLevel : groupLevel}>{"Attributes"}</Title>
    <Form.List name={(wrapper ? [wrapper] : []).concat(["attributes"])}>
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
                name={[name, "key"]}
                rules={[{ required: true, message: "Missing key" }]}
                labelAlign="left"
              >
                <Input placeholder="key" />
              </Form.Item>
              <Form.Item
                {...restField}
                name={[name, "value"]}
                labelAlign="left"
              >
                <Input placeholder="value" />
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
              Add attribute
            </Button>
          </Form.Item>
        </>
      )}
    </Form.List>
  </>
);
