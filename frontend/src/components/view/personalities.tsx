import { CloseOutlined, PlusOutlined } from "@ant-design/icons";
import { Button, Card, Form, Input } from "antd";
import { groupLevel, Title } from "./consts";

export const Personalities = () => (
  <>
    <Title level={groupLevel}>{"Personalities"}</Title>
    <Form.List name="personalities">
      {(fields, { add, remove }) => (
        <>
          {fields.map(({ key, name, ...restField }) => (
            <Form.Item>
              <Card
                size="small"
                title={`Personality ${name + 1}`}
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
                  label={"Wieght"}
                  name={[name, "weight"]}
                  rules={[{ required: true, message: "Missing weight" }]}
                >
                  <Input type="number" defaultValue={0} />
                </Form.Item>
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
              Add personality
            </Button>
          </Form.Item>
        </>
      )}
    </Form.List>
  </>
);
