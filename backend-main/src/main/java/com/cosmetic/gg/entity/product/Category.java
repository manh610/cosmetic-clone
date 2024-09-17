package com.cosmetic.gg.entity.product;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import com.cosmetic.gg.common.converter.StringListConverter;
import com.cosmetic.gg.entity.EntityCommon;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "category",
uniqueConstraints = { @UniqueConstraint(columnNames = { "code", "parent_id", "status" }) },
indexes = {
  @Index(name = "idx_category_code", columnList = "code"),
  @Index(name = "idx_category_name", columnList = "name")
})
@Entity
@Getter
@Setter
public class Category extends EntityCommon{
	private static final long serialVersionUID = 1L;

	@NotEmpty(message = "Code can not empty")
	@NotNull(message = "Code can not null")
	@Size(max = 50, message = "Max length is 50 characters")
	@Pattern(regexp = "^[a-zA-Z0-9_.-]*$", message = "Code only contains character, number, special character _-.")
	@Column(name = "code", length = 50)
	private String code;
	
	@NotEmpty(message = "Name can not empty")
	@NotNull(message = "Name can not null")
	@Size(max = 700, message = "Max length is 700 characters")
	@Column(name = "name", length = 700)
	private String name;
	
	@NotEmpty(message = "Path can not empty")
	@NotNull(message = "Path can not null")
	@Column(name = "path")
	private String path;
	
	@NotNull(message = "Offset can not null")
	@Column(name = "\"offset\"")
	private int offset;
	
	@Column(name = "parent_id")
	private String parentId;
	
	@Column(name = "icon")
	private String icon;
	
	@Column(name = "image")
	@Lob
    private byte[] image;
	
	@Size(max = 700, message = "Max length is 700 characters")
	@Column(name = "ancestors", length = 700)
	@Convert(converter = StringListConverter.class)
	private List<String> ancestors = new ArrayList<>();	
}
