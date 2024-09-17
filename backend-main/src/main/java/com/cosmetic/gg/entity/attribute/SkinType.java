package com.cosmetic.gg.entity.attribute;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Index;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import com.cosmetic.gg.entity.EntityBase;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "skin_type", indexes = {
		@Index(name = "idx_skin_type_name", columnList = "name")
})
@Entity
@Getter
@Setter
public class SkinType extends EntityBase{
	private static final long serialVersionUID = 1L;

	@NotEmpty(message = "Name can not empty")
	@NotNull(message = "Name can not null")
	@Size(max = 700, message = "Max length is 700 characters")
	@Column(name = "name", length = 700)
	private String name;
	
	@Column(name = "description", length = 700)
	private String description;
	
	@Column(name = "image")
	private String image;
}
