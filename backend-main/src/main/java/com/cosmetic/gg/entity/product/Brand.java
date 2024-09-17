package com.cosmetic.gg.entity.product;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Index;
import javax.persistence.Lob;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import com.cosmetic.gg.entity.EntityCommon;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "brand", indexes = {
    @Index(name = "idx_brand_code", columnList = "code"),
    @Index(name = "idx_brand_name", columnList = "name")
})
@Entity
@Getter
@Setter
public class Brand extends EntityCommon{
	private static final long serialVersionUID = 1L;

	@NotNull(message = "Code can not null")
	@Column(name = "code", unique = true)
	private String code;
	
	@NotNull(message = "Name can not null")
	@Column(name = "name")
	private String name;
	
	@NotNull(message = "Country can not null")
	@Column(name = "country")
	private String country;
	
	@Column(name = "logo")
	@Lob
	private byte[] logo;
	
	@Column(name = "slogan")
	private String slogan;
	
	@Column(name = "is_mall")
	private boolean isMall = false;
}
