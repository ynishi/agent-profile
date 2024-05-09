import { MinusCircleOutlined, PlusOutlined } from "@ant-design/icons";
import { TextField } from "@refinedev/antd";
import { Button, Form, Input, List, Space } from "antd";
import { groupLevel, Title } from "./consts";

export const Attributes = ({ value }: any) => (
  <>
    <Title level={groupLevel}>{"Attributes"}</Title>
    <List
      bordered
      dataSource={(value ?? []) as any[]}
      renderItem={(item) => (
        <List.Item>
          <TextField value={item.key} />
          <TextField value={item.value} />
        </List.Item>
      )}
    />
  </>
);
