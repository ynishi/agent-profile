import { List } from "antd";

import { Attributes } from "./showAttributes";
import { TextField } from "@refinedev/antd";
import { Title, groupLevel } from "./consts";

export const Policies = ({ value }: any) => (
  <>
    <Title level={groupLevel}>{"Policies"}</Title>
    <List
      bordered
      dataSource={(value ?? []) as any[]}
      renderItem={(item) => (
        <List.Item>
          <TextField value={item.name} />
          <TextField value={item.description} />
          <TextField value={item.content} />
          <TextField value={item.priority} />
          <Attributes value={item.attributes} />
        </List.Item>
      )}
    />
  </>
);
