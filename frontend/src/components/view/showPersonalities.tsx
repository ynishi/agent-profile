import { List } from "antd";

import { TextField } from "@refinedev/antd";
import { groupLevel, Title } from "./consts";

export const Personalities = ({ value }: any) => (
  <>
    <Title level={groupLevel}>{"Personalities"}</Title>
    <List
      bordered
      dataSource={(value ?? []) as any[]}
      renderItem={(item) => (
        <List.Item>
          <TextField value={item.name} />
          <TextField value={item.description} />
          <TextField value={item.content} />
          <TextField value={item.weight} />
        </List.Item>
      )}
    />
  </>
);
