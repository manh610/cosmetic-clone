package com.cosmetic.gg.entity.discount;

import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Lob;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import com.cosmetic.gg.common.enums.EDiscountType;
import com.cosmetic.gg.entity.EntityBase;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "discount")
@Entity
@Getter
@Setter
public class Discount extends EntityBase{
	
	@NotEmpty(message = "Code can not empty")
	@NotNull(message = "Code can not null")
	@Pattern(regexp = "^[a-zA-Z0-9_.-]*$", message = "Code only contains character, number, special character _-.")
	private String code;
	
	@NotEmpty(message = "Name can not empty")
	@NotNull(message = "Name can not null")
	@Size(max = 700, message = "Max length is 700 characters")
	@Column(name = "name")
	private String name;
	
	@Column(name = "start_date")
	private LocalDateTime startDate;
	
	@Column(name = "end_date")
	private LocalDateTime endDate;
	
	@Column(name = "discount_type")
	@Enumerated(EnumType.STRING)
	private EDiscountType discountType;
	
	@Column(name = "value")
	private Integer value;
	
	@Column(name = "path")
	private String path;
	
	@Column(name = "is_show")
	private boolean isShow;
	
	@Column(name = "image")
	@Lob
	private byte[] image;
	
	@Column(name = "description")
	private String description;
}
