export interface nodeTree {
  id:string;
  hasChild:boolean;
  name: string;
  expandable: boolean;
  children?: nodeTree[];
}

export interface treeDefault {
  id: string;
  hasChild: boolean;
  expandable: boolean;
  name: string;
  level: number;
}
//myData là kiểu dữ liệu người dùng theo bảng người dùng muốn thêm mở rộng cho treeview, hạn chế các trường define không có tính tái sử dụng
class myData {
  parentId!: string;
  description!: string;
  offset!: string;
  administrativeCode!: number;
}
export class TodoItemNode extends myData {
  children?: TodoItemNode[];
  code!: string;
  name!: string;
  id!: string;
  hasChild: boolean = false;
}

export class TodoItemFlatNode extends myData {
  code!: string;
  name!: string;
  id!: string;
  level!: number;
  expandable!: boolean;
  hasChild: boolean = false;
}
