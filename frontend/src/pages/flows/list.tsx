import {
  DeleteButton,
  EditButton,
  List,
  ShowButton,
  useTable,
} from "@refinedev/antd";
import { BaseRecord } from "@refinedev/core";
import { Space, Table } from "antd";

export const FlowList = () => {
  const { tableProps } = useTable({
    syncWithLocation: true,
  });

  return (
    <List>
      <Table {...tableProps} rowKey="flow_id">
        <Table.Column dataIndex="flow_id" title={"ID"} />
        <Table.Column dataIndex="name" title={"name"} />
        <Table.Column dataIndex="description" title={"description"} />
        <Table.Column
          title={"Actions"}
          dataIndex="actions"
          render={(_, record: BaseRecord) => (
            <Space>
              <EditButton hideText recordItemId={record.flow_id} />
              <ShowButton hideText recordItemId={record.flow_id} />
              <DeleteButton hideText recordItemId={record.flow_id} />
            </Space>
          )}
        />
      </Table>
    </List>
  );
};
