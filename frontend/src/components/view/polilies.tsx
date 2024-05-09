import { CloseOutlined, PlusOutlined } from "@ant-design/icons";
import { Button, Card, Form, Input } from "antd";

import { Attributes } from "./attributes";
import { groupLevel, Title } from "./consts";

export const Policies = () => (
  <>
    <Title level={groupLevel}>{"Policies"}</Title>
    <Form.List name="policies">
      {(fields, { add, remove }) => (
        <>
          {fields.map(({ key, name, ...restField }) => (
            <Form.Item>
              <Card
                size="small"
                title={`Policy ${name + 1}`}
                key={key}
                extra={
                  <CloseOutlined
                    onClick={() => {
                      remove(name);
                    }}
                  />
                }
              >
                <Form.Item
                  {...restField}
                  label={"Name"}
                  name={[name, "name"]}
                  rules={[{ required: true, message: "Missing name" }]}
                >
                  <Input />
                </Form.Item>
                <Form.Item
                  {...restField}
                  label={"Description"}
                  name={[name, "description"]}
                >
                  <Input />
                </Form.Item>
                <Form.Item
                  {...restField}
                  label={"Content"}
                  name={[name, "content"]}
                  rules={[{ required: true, message: "Missing content" }]}
                >
                  <Input />
                </Form.Item>
                <Form.Item
                  {...restField}
                  label={"Priority"}
                  name={[name, "priority"]}
                  rules={[{ required: true, message: "Missing priority" }]}
                >
                  <Input type="number" defaultValue={0} />
                </Form.Item>
                <Attributes wrapper={name} />
              </Card>
            </Form.Item>
          ))}
          <Form.Item>
            <Button
              type="dashed"
              onClick={() => add()}
              block
              icon={<PlusOutlined />}
            >
              Add policy
            </Button>
          </Form.Item>
        </>
      )}
    </Form.List>
  </>
);
