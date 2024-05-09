import {
  DeleteButton,
  EditButton,
  List,
  ShowButton,
  useTable,
} from "@refinedev/antd";
import { BaseRecord } from "@refinedev/core";
import { Space, Table } from "antd";

export const FunctionList = () => {
  const { tableProps } = useTable({
    syncWithLocation: true,
  });

  return (
    <List>
      <Table {...tableProps} rowKey="function_id">
        <Table.Column dataIndex="function_id" title={"ID"} />
        <Table.Column dataIndex="name" title={"name"} />
        <Table.Column dataIndex="description" title={"description"} />
        <Table.Column
          title={"Actions"}
          dataIndex="actions"
          render={(_, record: BaseRecord) => (
            <Space>
              <EditButton hideText recordItemId={record.function_id} />
              <ShowButton hideText recordItemId={record.function_id} />
              <DeleteButton hideText recordItemId={record.function_id} />
            </Space>
          )}
        />
      </Table>
    </List>
  );
};
